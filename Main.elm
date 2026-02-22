-- Copyright (C) 2026 Mitchell Dalvi Rosen
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General
-- Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along with this program. If not, see
-- <https://www.gnu.org/licenses/>.


port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, input, li, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autocomplete, class, href, id, placeholder, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, preventDefaultOn)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Set exposing (Set)
import Task
import Url exposing (Url)
import Url.Parser exposing ((</>))


port documentBecameVisible : (() -> msg) -> Sub msg


port saveModel : Value -> Cmd msg


port selectAll : String -> Cmd msg


type alias Model =
    { confirmingDelete : Bool
    , darkMode : Bool
    , dataVersion : Int
    , editingName : Maybe String
    , error : Maybe String
    , fabValue : Maybe String
    , itemStores : Dict ItemId (Dict StoreId ItemStore)
    , items : Dict ItemId Item
    , -- An items search index. Must be manually kept in-sync with items.
      itemsIndex : List ( String, Set ItemId )
    , key : Navigation.Key
    , -- On the list page, the set of items that were on the list when we tapped them (moving them off it)
      -- We remember these in a set so items don't jump when you move them off the list. This makes it easier to add
      -- back something that was accidentally removed.
      listPageJustMovedOffItems : Set ItemId
    , locations : Dict LocationId Location
    , page : Page
    , reorderingLocations : List LocationId
    , -- Outstanding HTTP requests. The front of the queue is in-flight.
      requests : Queue Request
    , shoppingPageLocationSelection : Maybe ItemId
    , storePageTab : Bool
    , stores : Dict StoreId Store
    , tapCount : Int
    }


type Page
    = ListPage
    | LocationPage LocationId
    | ItemPage ItemId
    | ItemStorePage ItemId StoreId
    | ItemsPage
    | ShoppingPage StoreId
    | ShoppingSelectionPage
    | StoreItemPage StoreId ItemId
    | StorePage StoreId
    | StoresPage


type Msg
    = AddItemToList String
    | AddItemToStore StoreId String
    | CreateLocation StoreId String
    | CreateStore String
    | Blur String
    | CancelSelectLocation
    | ConfirmDelete
    | CreateItem String
    | DocumentBecameVisible
    | DeleteItem ItemId
    | DeleteLocation Location
    | DeleteStore StoreId
    | DismissError
    | EditName
    | EditNameChanged String
    | EditNameSubmit
    | FabClose
    | FabOpen
    | FabValueChanged String
    | ItemInStore ItemId StoreId (Maybe LocationId)
    | ItemNotInStore ItemId StoreId
    | MoveItemOff ItemId (Maybe ( StoreId, Maybe LocationId ))
    | MoveItemOffAfterSelectingLocation ItemId
    | MoveItemOn ItemId
    | Noop
    | ReorderLocationsDone StoreId
    | ReorderLocationsMoveUp Int
    | ReorderLocationsStart StoreId
    | ReplaceUrl String
    | Response Response
    | SetStorePageTab Bool
    | ToggleDarkMode
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update =
            -- \msg model ->
            --     let
            --         ( model1, cmd ) =
            --             update (Debug.log "msg" msg) model
            --     in
            --     ( Debug.log "model" model1, cmd )
            update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


pageParser : Url.Parser.Parser (Page -> a) a
pageParser =
    Url.Parser.oneOf
        [ Url.Parser.map ListPage Url.Parser.top
        , Url.Parser.map ItemPage (Url.Parser.s "item" </> Url.Parser.int)
        , Url.Parser.map ItemStorePage (Url.Parser.s "item" </> Url.Parser.int </> Url.Parser.s "store" </> Url.Parser.int)
        , Url.Parser.map ItemsPage (Url.Parser.s "items")
        , Url.Parser.map LocationPage (Url.Parser.s "location" </> Url.Parser.int)
        , Url.Parser.map ShoppingPage (Url.Parser.s "shop" </> Url.Parser.int)
        , Url.Parser.map ShoppingSelectionPage (Url.Parser.s "shop")
        , Url.Parser.map StoreItemPage (Url.Parser.s "store" </> Url.Parser.int </> Url.Parser.s "item" </> Url.Parser.int)
        , Url.Parser.map StorePage (Url.Parser.s "store" </> Url.Parser.int)
        , Url.Parser.map StoresPage (Url.Parser.s "stores")
        ]


pageUrl : Page -> String
pageUrl page =
    case page of
        ListPage ->
            "/"

        ItemPage itemId ->
            "/item/" ++ String.fromInt itemId

        ItemStorePage itemId storeId ->
            "/item/" ++ String.fromInt itemId ++ "/store/" ++ String.fromInt storeId

        ItemsPage ->
            "/items"

        LocationPage locationId ->
            "/location/" ++ String.fromInt locationId

        ShoppingPage storeId ->
            "/shop/" ++ String.fromInt storeId

        ShoppingSelectionPage ->
            "/shop"

        StoreItemPage storeId itemId ->
            "/store/" ++ String.fromInt storeId ++ "/item/" ++ String.fromInt itemId

        StorePage storeId ->
            "/store/" ++ String.fromInt storeId

        StoresPage ->
            "/stores"


init : Value -> Url -> Navigation.Key -> Eff Model Msg
init flags url key =
    let
        dataVersion : Int
        dataVersion =
            0

        model : Page -> Model
        model page =
            { confirmingDelete = False
            , darkMode =
                Decode.decodeValue (Decode.field "dark_mode" Decode.bool) flags
                    |> Result.withDefault False
            , dataVersion = dataVersion
            , editingName = Nothing
            , error = Nothing
            , fabValue = Nothing
            , items = Dict.empty
            , itemsIndex = []
            , itemStores = Dict.empty
            , key = key
            , listPageJustMovedOffItems = Set.empty
            , locations = Dict.empty
            , page = page
            , reorderingLocations = []
            , requests = Queue [] []
            , shoppingPageLocationSelection = Nothing
            , storePageTab = False
            , stores = Dict.empty
            , tapCount = 0
            }
    in
    case Url.Parser.parse pageParser url of
        Just page ->
            pure (model page)
                |> andThen (enqueueRequest (RequestGetItems { dataVersion = dataVersion }))

        Nothing ->
            pure (model ListPage)


update : Msg -> Model -> Eff Model Msg
update msg model =
    case msg of
        AddItemToList name ->
            handleAddItemToList model name

        AddItemToStore storeId name ->
            handleAddItemToStore model storeId name

        Blur id ->
            handleBlur model id

        CancelSelectLocation ->
            handleCancelSelectLocation model

        ConfirmDelete ->
            handleConfirmDelete model

        CreateItem name ->
            handleCreateItem model name

        CreateLocation storeId name ->
            handleCreateLocation model storeId name

        CreateStore name ->
            handleCreateStore model name

        DocumentBecameVisible ->
            handleDocumentBecameVisible model

        DeleteItem id ->
            handleDeleteItem model id

        DeleteLocation location ->
            handleDeleteLocation model location

        DeleteStore id ->
            handleDeleteStore model id

        DismissError ->
            handleDismissError model

        EditName ->
            handleEditName model

        EditNameChanged name ->
            handleEditNameChanged model name

        EditNameSubmit ->
            handleEditNameSubmit model

        FabClose ->
            handleFabClose model

        FabOpen ->
            handleFabOpen model

        FabValueChanged value ->
            handleFabValueChanged model value

        ItemInStore itemId storeId maybeLocationId ->
            handleItemInStore model itemId storeId maybeLocationId

        ItemNotInStore itemId storeId ->
            handleItemNotInStore model itemId storeId

        MoveItemOff item storeAndLocation ->
            handleMoveItemOff model item storeAndLocation

        MoveItemOffAfterSelectingLocation item ->
            handleMoveItemOffAfterSelectingLocation model item

        MoveItemOn item ->
            handleMoveItemOn model item

        Noop ->
            pure model

        ReorderLocationsDone storeId ->
            handleReorderLocationsDone model storeId

        ReorderLocationsMoveUp index ->
            handleReorderLocationsMoveUp model index

        ReorderLocationsStart storeId ->
            handleReorderLocationsStart model storeId

        ReplaceUrl url ->
            handleReplaceUrl model url

        Response response ->
            handleResponse model response

        SetStorePageTab tab ->
            handleSetStorePageTab model tab

        ToggleDarkMode ->
            handleToggleDarkMode model

        UrlChanged url ->
            handleUrlChanged model url

        UrlRequested urlRequest ->
            handleUrlRequested model urlRequest


{-| Add an item to the list by name. It might already exist, in which case we'll just move it onto the list. Otherwise,
we'll create it.
-}
handleAddItemToList : Model -> String -> Eff Model Msg
handleAddItemToList model name =
    let
        name1 =
            String.trim name

        model1 =
            { model | fabValue = Nothing }
    in
    if String.isEmpty name1 then
        pure model1

    else
        case getItemByName model name1 of
            Nothing ->
                pure model1
                    |> andThen
                        (enqueueRequest
                            (RequestCreateItem
                                { name = name1
                                , onList = True
                                , store = Nothing
                                }
                            )
                        )

            Just item ->
                pure model1
                    |> andThenIf
                        (not item.onList)
                        (\model2 ->
                            enqueueRequest
                                (RequestMoveItemOn
                                    { item = item.id
                                    , store = Nothing
                                    }
                                )
                                model2
                        )


handleAddItemToStore : Model -> StoreId -> String -> Eff Model Msg
handleAddItemToStore model store name =
    let
        name1 =
            String.trim name

        model1 =
            { model | fabValue = Nothing }
    in
    if String.isEmpty name1 then
        pure model1

    else
        case getItemByName model name1 of
            Nothing ->
                pure model1
                    |> andThen
                        (enqueueRequest
                            (RequestCreateItem
                                { name = name1
                                , onList = False
                                , store = Just store
                                }
                            )
                        )

            Just item ->
                pure model1
                    |> andThenIf
                        (not (isItemKnownToBeSoldAtStore model store item.id))
                        (\model2 ->
                            enqueueRequest
                                (RequestItemInStore
                                    { item = item.id
                                    , store = store
                                    , location = Nothing
                                    }
                                )
                                model2
                        )


handleCreateItem : Model -> String -> Eff Model Msg
handleCreateItem model name =
    let
        name1 =
            String.trim name
    in
    pure { model | fabValue = Nothing }
        |> andThenIf
            (not (String.isEmpty name1) && isNothing (getItemByName model name1))
            (\model1 ->
                enqueueRequest
                    (RequestCreateItem
                        { name = name1
                        , onList = False
                        , store = Nothing
                        }
                    )
                    model1
            )


handleCreateLocation : Model -> StoreId -> String -> Eff Model Msg
handleCreateLocation model storeId name =
    let
        name1 : String
        name1 =
            String.trim name
    in
    pure { model | fabValue = Nothing }
        |> andThenIf
            (not (String.isEmpty name1))
            (\model1 ->
                enqueueRequest
                    (RequestCreateLocation
                        { store = storeId
                        , name = name1
                        }
                    )
                    model1
            )


handleCreateStore : Model -> String -> Eff Model Msg
handleCreateStore model name =
    let
        name1 : String
        name1 =
            String.trim name
    in
    pure { model | fabValue = Nothing }
        |> andThenIf
            (not (String.isEmpty name1))
            (\model1 -> enqueueRequest (RequestCreateStore { name = name1 }) model1)


handleBlur : Model -> String -> Eff Model Msg
handleBlur model id =
    pure model
        |> command (Task.attempt (\_ -> Noop) (Browser.Dom.blur id))


handleCancelSelectLocation : Model -> Eff Model msg
handleCancelSelectLocation model =
    pure { model | shoppingPageLocationSelection = Nothing }


handleConfirmDelete : Model -> Eff Model msg
handleConfirmDelete model =
    pure { model | confirmingDelete = True }


handleDocumentBecameVisible : Model -> Eff Model Msg
handleDocumentBecameVisible model =
    pure model
        |> andThen (enqueueRequest (RequestGetItems { dataVersion = model.dataVersion }))


handleDeleteItem : Model -> ItemId -> Eff Model Msg
handleDeleteItem model id =
    pure { model | confirmingDelete = False }
        |> andThen (enqueueRequest (RequestDeleteItem { id = id }))
        |> command (Navigation.replaceUrl model.key (pageUrl ItemsPage))


handleDeleteLocation : Model -> Location -> Eff Model Msg
handleDeleteLocation model location =
    pure { model | confirmingDelete = False }
        |> andThen (enqueueRequest (RequestDeleteLocation location.store { id = location.id }))
        |> command (Navigation.replaceUrl model.key (pageUrl (StorePage location.store)))


handleDeleteStore : Model -> StoreId -> Eff Model Msg
handleDeleteStore model id =
    pure { model | confirmingDelete = False }
        |> andThen (enqueueRequest (RequestDeleteStore { id = id }))
        |> command (Navigation.replaceUrl model.key (pageUrl StoresPage))


handleDismissError : Model -> Eff Model msg
handleDismissError model =
    pure { model | error = Nothing }


handleEditName : Model -> Eff Model Msg
handleEditName model =
    case model.editingName of
        Nothing ->
            case
                case model.page of
                    ItemPage itemId ->
                        Maybe.map .name (Dict.get itemId model.items)

                    LocationPage locationId ->
                        Maybe.map .name (Dict.get locationId model.locations)

                    StorePage storeId ->
                        Maybe.map .name (Dict.get storeId model.stores)

                    _ ->
                        Nothing
            of
                Just name ->
                    pure
                        { model
                            | confirmingDelete = False
                            , editingName = Just name
                        }
                        |> -- trying out not selecting all on focus
                           (if False then
                                command (selectAll "name-edit")

                            else
                                identity
                           )

                Nothing ->
                    pure { model | confirmingDelete = False }

        Just _ ->
            pure model


handleEditNameChanged : Model -> String -> Eff Model msg
handleEditNameChanged model name =
    pure { model | editingName = Just name }


handleEditNameSubmit : Model -> Eff Model Msg
handleEditNameSubmit model =
    case ( model.page, model.editingName ) of
        ( ItemPage itemId, Just newName ) ->
            let
                newName1 : String
                newName1 =
                    String.trim newName
            in
            if String.isEmpty newName1 then
                pure { model | editingName = Nothing }

            else
                case Dict.get itemId model.items of
                    Just item ->
                        if newName1 /= item.name then
                            pure { model | editingName = Nothing }
                                |> andThen
                                    (enqueueRequest
                                        (RequestRenameItem
                                            { id = itemId
                                            , name = newName1
                                            }
                                        )
                                    )

                        else
                            pure { model | editingName = Nothing }

                    Nothing ->
                        pure model

        ( LocationPage locationId, Just newName ) ->
            let
                newName1 : String
                newName1 =
                    String.trim newName
            in
            if String.isEmpty newName1 then
                pure { model | editingName = Nothing }

            else
                case Dict.get locationId model.locations of
                    Just location ->
                        if newName1 /= location.name then
                            pure
                                { model | editingName = Nothing }
                                |> andThen
                                    (enqueueRequest
                                        (RequestRenameLocation
                                            { id = locationId
                                            , store = location.store
                                            , name = newName1
                                            }
                                        )
                                    )

                        else
                            pure { model | editingName = Nothing }

                    Nothing ->
                        pure { model | editingName = Nothing }

        ( StorePage storeId, Just newName ) ->
            let
                newName1 : String
                newName1 =
                    String.trim newName
            in
            if String.isEmpty newName1 then
                pure { model | editingName = Nothing }

            else
                case Dict.get storeId model.stores of
                    Just store ->
                        if newName1 /= store.name then
                            pure { model | editingName = Nothing }
                                |> andThen
                                    (enqueueRequest
                                        (RequestRenameStore
                                            { id = storeId
                                            , name = newName1
                                            }
                                        )
                                    )

                        else
                            pure { model | editingName = Nothing }

                    Nothing ->
                        pure model

        _ ->
            pure model


handleFabClose : Model -> Eff Model msg
handleFabClose model =
    pure { model | fabValue = Nothing }


handleFabOpen : Model -> Eff Model Msg
handleFabOpen model =
    pure
        { model
            | confirmingDelete = False
            , fabValue = Just ""
        }
        |> command (selectAll "fab-input")


handleFabValueChanged : Model -> String -> Eff Model msg
handleFabValueChanged model value =
    pure { model | fabValue = Just value }


handleItemInStore : Model -> ItemId -> StoreId -> Maybe LocationId -> Eff Model Msg
handleItemInStore model itemId storeId maybeLocationId =
    pure model
        |> andThen
            (enqueueRequest
                (RequestItemInStore
                    { item = itemId
                    , store = storeId
                    , location = maybeLocationId
                    }
                )
            )
        -- If we're on the store+item location selector, as a convenience, navigate back
        |> command
            (case model.page of
                ItemStorePage item _ ->
                    Navigation.replaceUrl model.key (pageUrl (ItemPage item))

                StoreItemPage store _ ->
                    Navigation.replaceUrl model.key (pageUrl (StorePage store))

                _ ->
                    Cmd.none
            )


handleItemNotInStore : Model -> ItemId -> StoreId -> Eff Model Msg
handleItemNotInStore model itemId storeId =
    pure model
        |> andThen
            (enqueueRequest
                (RequestItemNotInStore
                    { item = itemId
                    , store = storeId
                    }
                )
            )
        -- If we're on the store+item location selector, as a convenience, navigate back
        |> command
            (case model.page of
                ItemStorePage item _ ->
                    Navigation.replaceUrl model.key (pageUrl (ItemPage item))

                StoreItemPage store _ ->
                    Navigation.replaceUrl model.key (pageUrl (StorePage store))

                _ ->
                    Cmd.none
            )


handleMoveItemOff : Model -> ItemId -> Maybe ( StoreId, Maybe LocationId ) -> Eff Model Msg
handleMoveItemOff model itemId storeAndLocation =
    let
        ( storeId, locationId ) =
            case storeAndLocation of
                Nothing ->
                    ( Nothing, Nothing )

                Just ( storeId1, locationId1 ) ->
                    ( Just storeId1, locationId1 )
    in
    pure model
        |> andThen
            (enqueueRequest
                (RequestMoveItemOff
                    { item = itemId
                    , store = storeId
                    , location = locationId
                    }
                )
            )


handleMoveItemOffAfterSelectingLocation : Model -> ItemId -> Eff Model msg
handleMoveItemOffAfterSelectingLocation model item =
    pure { model | shoppingPageLocationSelection = Just item }


handleMoveItemOn : Model -> ItemId -> Eff Model Msg
handleMoveItemOn model id =
    pure { model | fabValue = Nothing }
        |> andThen
            (enqueueRequest
                (RequestMoveItemOn
                    { item = id
                    , store = Nothing
                    }
                )
            )


handleResponse : Model -> Response -> Eff Model Msg
handleResponse model response =
    (case response of
        ResponseCreateItem request result ->
            handleResponseCreateItem model request result

        ResponseCreateLocation request result ->
            handleResponseCreateLocation model request result

        ResponseCreateStore request result ->
            handleResponseCreateStore model request result

        ResponseDeleteItem request result ->
            handleResponseDeleteItem model request result

        ResponseDeleteLocation storeId request result ->
            handleResponseDeleteLocation model storeId request result

        ResponseDeleteStore request result ->
            handleResponseDeleteStore model request result

        ResponseGetItems result ->
            handleResponseGetItems model result

        ResponseItemInStore request result ->
            handleResponseItemInStore model request result

        ResponseItemNotInStore request result ->
            handleResponseItemNotInStore model request result

        ResponseMoveItemOff request result ->
            handleResponseMoveItemOff model request result

        ResponseMoveItemOn request result ->
            handleResponseMoveItemOn model request result

        ResponseRenameItem request result ->
            handleResponseRenameItem model request result

        ResponseRenameLocation request result ->
            handleResponseRenameLocation model request result

        ResponseRenameStore request result ->
            handleResponseRenameStore model request result

        ResponseReorderLocations request result ->
            handleResponseReorderLocations model request result
    )
        |> purely dropRequest
        |> andThen fireOffNextRequest


handleResponseCreateItem : Model -> CreateItemRequest -> Result Http.Error CreateItemResponse -> Eff Model Msg
handleResponseCreateItem model request result =
    case result of
        Ok response ->
            let
                items =
                    Dict.insert
                        response.id
                        { id = response.id
                        , name = request.name
                        , onList = request.onList
                        }
                        model.items
            in
            pure
                { model
                    | items = items
                    , itemsIndex = buildItemsIndex (Dict.values items)
                    , itemStores =
                        case request.store of
                            Nothing ->
                                model.itemStores

                            Just store ->
                                Dict.insert
                                    response.id
                                    (Dict.singleton
                                        store
                                        { item = response.id
                                        , store = store
                                        , sold = True
                                        , location = Nothing
                                        }
                                    )
                                    model.itemStores
                }
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseCreateStore : Model -> CreateStoreRequest -> Result Http.Error CreateStoreResponse -> Eff Model Msg
handleResponseCreateStore model request result =
    case result of
        Ok response ->
            pure
                { model
                    | stores =
                        Dict.insert
                            response.id
                            { id = response.id
                            , name = request.name
                            }
                            model.stores
                }
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseCreateLocation :
    Model
    -> CreateLocationRequest
    -> Result Http.Error CreateLocationResponse
    -> Eff Model Msg
handleResponseCreateLocation model request result =
    case result of
        Ok response ->
            pure
                { model
                    | locations =
                        Dict.insert
                            response.id
                            { id = response.id
                            , store = request.store
                            , position = response.position
                            , name = request.name
                            }
                            model.locations
                }
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseDeleteItem : Model -> DeleteItemRequest -> Result Http.Error DeleteItemResponse -> Eff Model Msg
handleResponseDeleteItem model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeDeleteItemRequestWillSucceed request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseDeleteLocation :
    Model
    -> StoreId
    -> DeleteLocationRequest
    -> Result Http.Error DeleteLocationResponse
    -> Eff Model Msg
handleResponseDeleteLocation model storeId request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeDeleteLocationRequestWillSucceed storeId request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseDeleteStore : Model -> DeleteStoreRequest -> Result Http.Error DeleteStoreResponse -> Eff Model Msg
handleResponseDeleteStore model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeDeleteStoreRequestWillSucceed request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseGetItems : Model -> Result Http.Error GetItemsResponse -> Eff Model msg
handleResponseGetItems model result =
    case result of
        Ok response ->
            let
                items =
                    List.foldl
                        (\item acc -> Dict.insert item.id item acc)
                        Dict.empty
                        response.items
            in
            pure
                { model
                    | dataVersion = response.dataVersion
                    , items = items
                    , itemsIndex = buildItemsIndex (Dict.values items)
                    , itemStores =
                        List.foldl
                            (\itemStore acc ->
                                dictUpsert
                                    itemStore.item
                                    (Maybe.withDefault Dict.empty >> Dict.insert itemStore.store itemStore)
                                    acc
                            )
                            Dict.empty
                            response.itemStores
                    , locations =
                        List.foldl
                            (\location acc -> Dict.insert location.id location acc)
                            Dict.empty
                            response.locations
                    , stores =
                        List.foldl
                            (\store acc -> Dict.insert store.id store acc)
                            Dict.empty
                            response.stores
                }

        Err err ->
            case err of
                Http.BadStatus 304 ->
                    pure model

                _ ->
                    pure { model | error = Just "Something went wrong." }


handleResponseItemInStore : Model -> ItemInStoreRequest -> Result Http.Error ItemInStoreResponse -> Eff Model Msg
handleResponseItemInStore model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeItemInStoreRequestWillSucceed request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseItemNotInStore :
    Model
    -> ItemNotInStoreRequest
    -> Result Http.Error ItemNotInStoreResponse
    -> Eff Model Msg
handleResponseItemNotInStore model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeItemNotInStoreRequestWillSucceed request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseMoveItemOff : Model -> MoveItemOffRequest -> Result Http.Error MoveItemOffResponse -> Eff Model Msg
handleResponseMoveItemOff model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeMoveItemOffRequestWillSucceed request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseMoveItemOn : Model -> MoveItemOnRequest -> Result Http.Error MoveItemOnResponse -> Eff Model Msg
handleResponseMoveItemOn model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeMoveItemOnRequestWillSucceed request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseRenameItem :
    Model
    -> RenameItemRequest
    -> Result Http.Error RenameItemResponse
    -> Eff Model Msg
handleResponseRenameItem model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeRenameItemRequestWillSucceed request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            -- Put the old name back, since we optimistically assumed the rename would succeed
            pure { model | error = Just "Something went wrong." }


handleResponseRenameLocation :
    Model
    -> RenameLocationRequest
    -> Result Http.Error RenameLocationResponse
    -> Eff Model Msg
handleResponseRenameLocation model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeRenameLocationRequestWillSucceed request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseRenameStore :
    Model
    -> RenameStoreRequest
    -> Result Http.Error RenameStoreResponse
    -> Eff Model Msg
handleResponseRenameStore model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeRenameStoreRequestWillSucceed request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleResponseReorderLocations :
    Model
    -> ReorderLocationsRequest
    -> Result Http.Error ReorderLocationsResponse
    -> Eff Model Msg
handleResponseReorderLocations model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeReorderLocationsRequestWillSucceed request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleReorderLocationsDone : Model -> StoreId -> Eff Model Msg
handleReorderLocationsDone model storeId =
    case model.reorderingLocations of
        _ :: _ :: _ ->
            let
                currentLocations =
                    getStoreLocations model storeId |> List.sortBy .position |> List.map .id
            in
            if model.reorderingLocations /= currentLocations then
                pure { model | reorderingLocations = [] }
                    |> andThen
                        (enqueueRequest
                            (RequestReorderLocations
                                { store = storeId
                                , locations = model.reorderingLocations
                                }
                            )
                        )

            else
                pure { model | reorderingLocations = [] }

        _ ->
            pure model


handleReorderLocationsMoveUp : Model -> Int -> Eff Model msg
handleReorderLocationsMoveUp model index =
    pure
        { model
            | reorderingLocations =
                if index == 0 then
                    case model.reorderingLocations of
                        location :: locations ->
                            locations ++ [ location ]

                        -- impossible
                        [] ->
                            model.reorderingLocations

                else
                    case List.drop (index - 1) model.reorderingLocations of
                        location1 :: location2 :: locations ->
                            List.take (index - 1) model.reorderingLocations ++ (location2 :: location1 :: locations)

                        -- impossible
                        _ ->
                            model.reorderingLocations
        }


handleReorderLocationsStart : Model -> StoreId -> Eff Model msg
handleReorderLocationsStart model storeId =
    pure
        { model
            | reorderingLocations =
                getStoreLocations model storeId
                    |> List.sortBy .position
                    |> List.map .id
        }


handleReplaceUrl : Model -> String -> Eff Model Msg
handleReplaceUrl model url =
    pure model
        |> command (Navigation.replaceUrl model.key url)


handleToggleDarkMode : Model -> Eff Model Msg
handleToggleDarkMode model =
    pure { model | darkMode = not model.darkMode }
        |> command (saveModel (Encode.object [ ( "dark_mode", Encode.bool (not model.darkMode) ) ]))


handleSetStorePageTab : Model -> Bool -> Eff Model msg
handleSetStorePageTab model tab =
    pure
        { model
            | reorderingLocations = []
            , storePageTab = tab
        }


handleUrlChanged : Model -> Url -> Eff Model Msg
handleUrlChanged model url =
    case Url.Parser.parse pageParser url of
        Just page ->
            pure
                { model
                    | page = page
                    , confirmingDelete = False
                    , fabValue = Nothing
                    , listPageJustMovedOffItems = Set.empty
                    , reorderingLocations = []
                }
                |> command (Task.perform (\_ -> Noop) (Browser.Dom.setViewport 0 0))

        Nothing ->
            pure model


handleUrlRequested : Model -> Browser.UrlRequest -> Eff Model Msg
handleUrlRequested model urlRequest =
    case urlRequest of
        Browser.Internal url ->
            case Url.Parser.parse pageParser url of
                Just page ->
                    let
                        action =
                            case page of
                                ListPage ->
                                    Navigation.replaceUrl

                                LocationPage _ ->
                                    Navigation.pushUrl

                                ItemPage _ ->
                                    Navigation.pushUrl

                                ItemStorePage _ _ ->
                                    Navigation.pushUrl

                                ItemsPage ->
                                    Navigation.replaceUrl

                                ShoppingPage _ ->
                                    Navigation.pushUrl

                                ShoppingSelectionPage ->
                                    Navigation.pushUrl

                                StoreItemPage _ _ ->
                                    Navigation.pushUrl

                                StorePage _ ->
                                    Navigation.pushUrl

                                StoresPage ->
                                    Navigation.replaceUrl
                    in
                    pure model
                        |> command (action model.key (Url.toString url))

                Nothing ->
                    pure model

        Browser.External href ->
            pure model
                |> command (Navigation.load href)


subscriptions : Model -> Sub Msg
subscriptions _ =
    documentBecameVisible (\_ -> DocumentBecameVisible)


view : Model -> Browser.Document Msg
view model =
    { title = "Shopping"
    , body = [ view1 (not (queueIsEmpty model.requests)) (assumeInFlightRequestsWillSucceed model) ]
    }


view1 : Bool -> Model -> Html Msg
view1 loading model =
    div
        [ class
            (if model.darkMode then
                "layout dark"

             else
                "layout"
            )
        ]
        (let
            showSidebar : Bool
            showSidebar =
                case model.page of
                    ShoppingPage _ ->
                        False

                    _ ->
                        True
         in
         [ if showSidebar then
            viewSidebar model.page model.darkMode

           else
            text ""
         , div
            [ class
                (if loading then
                    "content loading"

                 else
                    "content"
                )
            ]
            ([ case model.error of
                Just err ->
                    viewErrorBanner err

                Nothing ->
                    text ""
             , div [ class "page" ] (viewPage model)
             ]
                ++ viewFab model
            )
         ]
        )


viewErrorBanner : String -> Html Msg
viewErrorBanner err =
    div
        [ class "error-banner" ]
        [ span [] [ text err ]
        , button [ onClick DismissError ] [ text "✕" ]
        ]


viewSidebar : Page -> Bool -> Html Msg
viewSidebar page darkMode =
    Html.nav
        [ class "sidebar" ]
        [ Html.a
            [ href (pageUrl ShoppingSelectionPage)
            , class
                (if page == ShoppingSelectionPage then
                    "sidebar-shop active"

                 else
                    "sidebar-shop"
                )
            ]
            [ span [ class "sidebar-shop-pill" ]
                [ span [ class "material-symbols-outlined fill" ] [ text "shopping_cart" ]
                , span [ class "sidebar-pill-label" ] [ text "Shop" ]
                ]
            ]
        , Html.hr [ class "sidebar-divider" ] []
        , Html.a
            [ href "/"
            , class
                (if page == ListPage then
                    "sidebar-list active"

                 else
                    "sidebar-list"
                )
            ]
            [ span [ class "sidebar-list-pill" ]
                [ span [ class "material-symbols-outlined fill" ] [ text "list" ]
                , span [ class "sidebar-pill-label" ] [ text "List" ]
                ]
            ]
        , Html.hr [ class "sidebar-divider-large" ] []
        , Html.a
            [ href (pageUrl ItemsPage)
            , class
                (case page of
                    ItemsPage ->
                        "active"

                    ItemPage _ ->
                        "active"

                    ItemStorePage _ _ ->
                        "active"

                    _ ->
                        ""
                )
            ]
            [ text "Items" ]
        , Html.a
            [ href (pageUrl StoresPage)
            , class
                (case page of
                    StoresPage ->
                        "active"

                    StorePage _ ->
                        "active"

                    StoreItemPage _ _ ->
                        "active"

                    _ ->
                        ""
                )
            ]
            [ text "Stores" ]
        , Html.hr [ class "sidebar-divider" ] []
        , button
            [ class "dark-mode-toggle"
            , onClick ToggleDarkMode
            ]
            [ span [ class "material-symbols-outlined" ]
                [ text
                    (if darkMode then
                        "light_mode"

                     else
                        "dark_mode"
                    )
                ]
            ]
        ]


viewFab : Model -> List (Html Msg)
viewFab model =
    case model.page of
        ListPage ->
            viewListFab model

        LocationPage _ ->
            []

        ItemPage _ ->
            viewStoreFab model

        ItemStorePage _ storeId ->
            viewLocationFab model storeId

        ItemsPage ->
            viewFabWith
                model
                "milk"
                CreateItem
                (fabItemSuggestions model (\_ -> FabClose))

        StoreItemPage storeId _ ->
            viewLocationFab model storeId

        StorePage storeId ->
            if model.storePageTab then
                viewLocationFab model storeId

            else
                viewFabWith
                    model
                    "milk"
                    (AddItemToStore storeId)
                    -- FIXME we can be more efficient than AddItemToStore here
                    (fabItemSuggestions model (\item -> AddItemToStore storeId item.name))

        StoresPage ->
            viewStoreFab model

        ShoppingPage storeId ->
            case model.shoppingPageLocationSelection of
                Nothing ->
                    viewFabWith
                        model
                        "milk"
                        -- We don't necessarily want to add to store even though we're shopping there, because we might
                        -- be thinking of an item to add to the list without yet knowing if it's sold there
                        AddItemToList
                        -- FIXME we can be more efficient than AddItemToList  here
                        (fabItemSuggestions model (\item -> AddItemToList item.name))

                Just _ ->
                    viewLocationFab model storeId

        ShoppingSelectionPage ->
            if Dict.isEmpty model.stores then
                viewListFab model

            else
                viewStoreFab model


viewListFab : Model -> List (Html Msg)
viewListFab model =
    viewFabWith
        model
        "milk"
        AddItemToList
        (fabItemSuggestions
            model
            (\item ->
                if item.onList then
                    FabClose

                else
                    MoveItemOn item.id
            )
        )


viewLocationFab : Model -> StoreId -> List (Html Msg)
viewLocationFab model storeId =
    viewFabWith model "Aisle 1" (CreateLocation storeId) []


viewStoreFab : Model -> List (Html Msg)
viewStoreFab model =
    viewFabWith model "Star Market" CreateStore []


fabItemSuggestions : Model -> (Item -> Msg) -> List ( String, Msg )
fabItemSuggestions model msg =
    case model.fabValue of
        Nothing ->
            []

        Just value ->
            if String.isEmpty value then
                []

            else
                searchItemsIndex value model.itemsIndex
                    |> Set.foldl
                        (\itemId items ->
                            case Dict.get itemId model.items of
                                Just item ->
                                    item :: items

                                Nothing ->
                                    items
                        )
                        []
                    |> List.sortBy .name
                    |> List.take 5
                    |> List.map (\item -> ( item.name, msg item ))


viewFabWith : Model -> String -> (String -> Msg) -> List ( String, Msg ) -> List (Html Msg)
viewFabWith model plc onSubmitMsg suggestions =
    [ case model.fabValue of
        Nothing ->
            text ""

        Just _ ->
            div [ class "scrim", onClick FabClose ] []
    , div
        [ class "fab" ]
        [ if List.isEmpty suggestions then
            text ""

          else
            div
                [ class "fab-suggestions" ]
                (List.map
                    (\( name, msg ) ->
                        div
                            [ class "fab-suggestion"
                            , -- Prevent browser default of un-selecting the input (which triggers FabClose)
                              preventDefaultOn "mousedown" (Decode.succeed ( msg, True ))
                            ]
                            [ text name ]
                    )
                    suggestions
                )
        , input
            [ attribute "autocapitalize" "none"
            , autocomplete False
            , class
                (case model.fabValue of
                    Nothing ->
                        "invisible"

                    Just _ ->
                        ""
                )
            , id "fab-input"
            , onBlur FabClose
            , onInput FabValueChanged
            , placeholder plc
            , preventDefaultOn
                "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\key ->
                            case key of
                                "Enter" ->
                                    Decode.succeed ( onSubmitMsg (Maybe.withDefault "" model.fabValue), True )

                                "Escape" ->
                                    Decode.succeed ( FabClose, True )

                                _ ->
                                    Decode.fail ""
                        )
                )
            , type_ "text"
            , value (Maybe.withDefault "" model.fabValue)
            ]
            []
        , case model.fabValue of
            Nothing ->
                span
                    [ class "fab-button"
                    , onClick FabOpen
                    ]
                    [ span [ class "material-symbols-outlined" ] [ text "add" ] ]

            Just _ ->
                text ""
        ]
    ]


viewPage : Model -> List (Html Msg)
viewPage model =
    case model.page of
        ListPage ->
            viewListPage model

        LocationPage locationId ->
            case Dict.get locationId model.locations of
                Just location ->
                    viewLocationPage model location

                Nothing ->
                    []

        ItemPage itemId ->
            case Dict.get itemId model.items of
                Just item ->
                    viewItemPage model item

                Nothing ->
                    []

        ItemStorePage itemId storeId ->
            case ( Dict.get storeId model.stores, Dict.get itemId model.items ) of
                ( Just store, Just item ) ->
                    viewStoreAndItemPageHelper model (ItemPage itemId) store item

                _ ->
                    []

        ItemsPage ->
            viewItemsPage model

        ShoppingPage storeId ->
            case Dict.get storeId model.stores of
                Just store ->
                    viewShoppingPage model store

                Nothing ->
                    []

        ShoppingSelectionPage ->
            viewShoppingSelectionPage model

        StoreItemPage storeId itemId ->
            case ( Dict.get storeId model.stores, Dict.get itemId model.items ) of
                ( Just store, Just item ) ->
                    viewStoreAndItemPageHelper model (StorePage storeId) store item

                _ ->
                    []

        StorePage storeId ->
            case Dict.get storeId model.stores of
                Just store ->
                    viewStorePage model store

                Nothing ->
                    []

        StoresPage ->
            viewStoresPage model


viewListPage : Model -> List (Html Msg)
viewListPage model =
    let
        allItems : List Item
        allItems =
            Dict.values model.items

        ( upperAreaItems, lowerAreaItems ) =
            List.partition
                (\item -> item.onList || Set.member item.id model.listPageJustMovedOffItems)
                allItems

        viewOffListItem : Item -> Html Msg
        viewOffListItem item =
            li
                [ onClick (MoveItemOn item.id) ]
                [ div [ class "muted" ]
                    [ text item.name
                    , smallMutedSymbol "add"
                    ]
                ]

        viewUpperAreaItem : Item -> Html Msg
        viewUpperAreaItem item =
            if Set.member item.id model.listPageJustMovedOffItems then
                viewOffListItem item

            else
                li
                    [ onClick (MoveItemOff item.id Nothing) ]
                    [ div [] [ text item.name ] ]
    in
    [ div
        [ class "page-header" ]
        [ smallMutedSymbol "list"
        , h1 [] [ text "List" ]
        ]
    , case upperAreaItems of
        [] ->
            text ""

        _ ->
            upperAreaItems
                |> List.sortBy (.name >> String.toLower)
                |> List.map viewUpperAreaItem
                |> ul []
    , case lowerAreaItems of
        [] ->
            text ""

        _ ->
            lowerAreaItems
                |> List.sortBy (.name >> String.toLower)
                |> List.map viewOffListItem
                |> ul []
    ]


viewLocationPage : Model -> Location -> List (Html Msg)
viewLocationPage model location =
    [ div
        [ class "page-header" ]
        [ Html.a
            [ href (pageUrl (StorePage location.store)) ]
            [ smallMutedSymbol "arrow_back" ]
        , h1 [] [ editNameInput (Maybe.withDefault location.name model.editingName) ]
        , if model.confirmingDelete then
            button
                [ class "no"
                , onClick (DeleteLocation location)
                ]
                [ span
                    [ class "material-symbols-outlined icon-sm" ]
                    [ text "delete" ]
                , text "Delete"
                ]

          else
            button
                [ onClick ConfirmDelete ]
                [ smallMutedSymbol "delete" ]
        ]
    ]


viewItemPage : Model -> Item -> List (Html Msg)
viewItemPage model item =
    let
        itemStores : Dict StoreId ItemStore
        itemStores =
            Dict.get item.id model.itemStores |> Maybe.withDefault Dict.empty

        viewStore : Store -> Html Msg
        viewStore store =
            let
                url =
                    pageUrl (ItemStorePage item.id store.id)

                chevron =
                    smallMutedSymbol "chevron_right"
            in
            li
                []
                [ case Dict.get store.id itemStores of
                    Just itemStore ->
                        if itemStore.sold then
                            Html.a
                                [ href url ]
                                [ span [] [ text store.name, chevron ]
                                , case itemStore.location of
                                    Nothing ->
                                        text ""

                                    Just location ->
                                        viewLocationLabel model location
                                ]

                        else
                            Html.a
                                [ class "muted"
                                , href url
                                ]
                                [ span [] [ span [ class "strikethrough" ] [ text store.name ], chevron ] ]

                    Nothing ->
                        Html.a
                            [ href url ]
                            [ span [] [ text (store.name ++ "?"), chevron ] ]
                ]
    in
    [ div
        [ class "page-header" ]
        [ Html.a
            [ href (pageUrl ItemsPage) ]
            [ smallMutedSymbol "arrow_back" ]
        , h1 [] [ editNameInput (Maybe.withDefault item.name model.editingName) ]
        , if model.confirmingDelete then
            button
                [ class "no"
                , onClick (DeleteItem item.id)
                ]
                [ span
                    [ class "material-symbols-outlined icon-sm" ]
                    [ text "delete" ]
                , text "Delete"
                ]

          else
            button
                [ onClick ConfirmDelete ]
                [ smallMutedSymbol "delete" ]
        ]
    , model.stores
        |> Dict.values
        |> List.sortBy (.name >> String.toLower)
        |> List.map viewStore
        |> ul []
    ]


viewItemsPage : Model -> List (Html Msg)
viewItemsPage model =
    let
        viewItem : Item -> Html Msg
        viewItem item =
            li
                []
                [ Html.a
                    [ href (pageUrl (ItemPage item.id)) ]
                    [ span
                        []
                        [ text item.name
                        , smallMutedSymbol "chevron_right"
                        ]
                    ]
                ]
    in
    [ div
        []
        [ div [ class "page-header" ] [ h1 [] [ text "Items" ] ]
        , ul
            []
            (model.items
                |> Dict.values
                |> List.sortBy (.name >> String.toLower)
                |> List.map viewItem
            )
        ]
    ]


{-| This is obviously a horrendous type, FIXME
-}
type ABC
    = A -- False True
    | B -- True False
    | C -- True True


viewShoppingPage : Model -> Store -> List (Html Msg)
viewShoppingPage model store =
    case model.shoppingPageLocationSelection of
        Nothing ->
            let
                simple : Bool
                simple =
                    not (doesStoreHaveLocations model store.id)

                f :
                    ItemId
                    -> Item
                    -> ( List ( Item, ABC ), Dict LocationId (List Item) )
                    -> ( List ( Item, ABC ), Dict LocationId (List Item) )
                f itemId item (( acc1, acc2 ) as acc) =
                    if item.onList then
                        let
                            maybeItemStore =
                                model.itemStores |> Dict.get itemId |> Maybe.andThen (Dict.get store.id)
                        in
                        case maybeItemStore of
                            Just itemStore ->
                                if itemStore.sold then
                                    case itemStore.location of
                                        Nothing ->
                                            ( ( item
                                              , if simple then
                                                    C

                                                else
                                                    B
                                              )
                                                :: acc1
                                            , acc2
                                            )

                                        Just location ->
                                            ( acc1
                                            , dictUpsert location (Maybe.withDefault [] >> (::) item) acc2
                                            )

                                else
                                    acc

                            Nothing ->
                                ( ( item, A ) :: acc1, acc2 )

                    else
                        acc

                ( locationlessItems, locationfulItems ) =
                    Dict.foldl f ( [], Dict.empty ) model.items
            in
            viewShoppingPageContents
                model
                (Just store)
                locationlessItems
                locationfulItems

        Just itemId ->
            viewShoppingPageLocationSelection model store itemId


viewShoppingPageLocationSelection : Model -> Store -> ItemId -> List (Html Msg)
viewShoppingPageLocationSelection model store itemId =
    case Dict.get itemId model.items of
        Just item ->
            viewShoppingPageLocationSelection1 model store item

        Nothing ->
            []


viewShoppingPageLocationSelection1 : Model -> Store -> Item -> List (Html Msg)
viewShoppingPageLocationSelection1 model store item =
    let
        locations : List Location
        locations =
            getStoreLocations model store.id

        viewLocation : Location -> Html Msg
        viewLocation location =
            li
                []
                [ div
                    [ onClick (MoveItemOff item.id (Just ( store.id, Just location.id ))) ]
                    [ text location.name ]
                ]
    in
    [ div
        [ class "page-header" ]
        [ span
            [ class "material-symbols-outlined icon-sm muted cursor-pointer"
            , onClick CancelSelectLocation
            ]
            [ text "arrow_back" ]
        , h1 [] [ text (item.name ++ " at " ++ store.name) ]
        ]
    , ul
        []
        ((locations
            |> List.sortBy (.name >> String.toLower)
            |> List.map viewLocation
         )
            ++ [ li
                    []
                    [ div
                        [ onClick (MoveItemOff item.id (Just ( store.id, Nothing ))) ]
                        [ text
                            (if List.isEmpty locations then
                                "Sold here"

                             else
                                "Somewhere"
                            )
                        ]
                    ]
               , li
                    []
                    [ div
                        [ onClick (ItemNotInStore item.id store.id) ]
                        [ text "Not sold here" ]
                    ]
               ]
        )
    ]


viewShoppingPageContents :
    Model
    -> Maybe Store
    -> List ( Item, ABC )
    -> Dict LocationId (List Item)
    -> List (Html Msg)
viewShoppingPageContents model maybeStore locationlessItems locationfulItems =
    let
        viewSeparator : String -> Html Msg
        viewSeparator name =
            div
                [ class "shopping-separator" ]
                [ div [ class "shopping-separator-line" ] []
                , span [ class "shopping-separator-label" ] [ text name ]
                , div [ class "shopping-separator-line" ] []
                ]
    in
    [ div
        [ class "page-header" ]
        [ span
            [ class "material-symbols-outlined fill icon-sm muted" ]
            [ text "shopping_cart" ]
        , h1 []
            [ text
                (case maybeStore of
                    Just store ->
                        store.name

                    Nothing ->
                        "Shop"
                )
            ]
        ]
    , if List.isEmpty locationlessItems then
        text ""

      else
        ul
            [ class "shop-items no-location" ]
            (locationlessItems
                |> List.sortBy (\( item, _ ) -> String.toLower item.name)
                |> List.map (\( item, abc ) -> viewShoppingPageItem item abc)
            )
    ]
        ++ (locationfulItems
                |> Dict.toList
                |> List.filterMap
                    (\( locationId, items ) ->
                        model.locations
                            |> Dict.get locationId
                            |> Maybe.map (\location -> ( location, items ))
                    )
                |> List.sortBy (Tuple.first >> .position)
                |> List.concatMap
                    (\( location, items ) ->
                        [ viewSeparator location.name
                        , ul
                            [ class "shop-items" ]
                            (items
                                |> List.sortBy (.name >> String.toLower)
                                |> List.map (\item -> viewShoppingPageItem item C)
                            )
                        ]
                    )
           )
        ++ [ Html.a
                [ href "/"
                , class "shopping-done"
                ]
                [ span
                    [ class "material-symbols-outlined icon-sm" ]
                    [ text "check_circle" ]
                , text "All done!"
                ]
           ]


viewShoppingPageItem : Item -> ABC -> Html Msg
viewShoppingPageItem item abc =
    li
        [ onClick
            (case abc of
                C ->
                    MoveItemOff item.id Nothing

                _ ->
                    MoveItemOffAfterSelectingLocation item.id
            )
        ]
        [ div []
            [ text
                (case abc of
                    A ->
                        item.name ++ "?"

                    _ ->
                        item.name
                )
            ]
        ]


viewShoppingSelectionPage : Model -> List (Html Msg)
viewShoppingSelectionPage model =
    if Dict.isEmpty model.stores then
        let
            f : ItemId -> Item -> List ( Item, ABC ) -> List ( Item, ABC )
            f _ item items =
                if item.onList then
                    ( item, C ) :: items

                else
                    items
        in
        viewShoppingPageContents
            model
            Nothing
            (Dict.foldl f [] model.items)
            Dict.empty

    else
        [ div
            [ class "page-header" ]
            [ span
                [ class "material-symbols-outlined fill icon-sm muted" ]
                [ text "shopping_cart" ]
            , h1 [] [ text "Shop" ]
            ]
        , model.stores
            |> Dict.values
            |> List.sortBy (.name >> String.toLower)
            |> List.map
                (\store ->
                    li
                        []
                        [ Html.a
                            [ href (pageUrl (ShoppingPage store.id)) ]
                            [ text store.name ]
                        ]
                )
            |> ul []
        ]


viewStorePage : Model -> Store -> List (Html Msg)
viewStorePage model store =
    [ div
        [ class "page-header" ]
        [ Html.a
            [ href (pageUrl StoresPage) ]
            [ span
                [ class "material-symbols-outlined icon-sm muted" ]
                [ text "arrow_back" ]
            ]
        , h1 [] [ editNameInput (Maybe.withDefault store.name model.editingName) ]
        , if model.confirmingDelete then
            button
                [ class "no"
                , onClick (DeleteStore store.id)
                ]
                [ span
                    [ class "material-symbols-outlined icon-sm" ]
                    [ text "delete" ]
                , text "Delete"
                ]

          else
            button
                [ onClick ConfirmDelete ]
                [ smallMutedSymbol "delete" ]
        ]
    , div
        [ class "tabs" ]
        [ button
            [ class
                (if not model.storePageTab then
                    "active"

                 else
                    ""
                )
            , onClick (SetStorePageTab False)
            ]
            [ text "Items" ]
        , button
            [ class
                (if model.storePageTab then
                    "active"

                 else
                    ""
                )
            , onClick (SetStorePageTab True)
            ]
            [ text "Sections" ]
        ]
    , if model.storePageTab then
        case model.reorderingLocations of
            _ :: _ :: _ ->
                div
                    []
                    [ viewStorePageLocationTabReorder model
                    , button
                        [ class "fab-button reorder-button"
                        , onClick (ReorderLocationsDone store.id)
                        ]
                        [ smallMutedSymbol "edit_off" ]
                    ]

            _ ->
                let
                    deletingLocation : Bool
                    deletingLocation =
                        queueContains
                            (\request ->
                                case request of
                                    RequestDeleteLocation storeId _ ->
                                        storeId == store.id

                                    _ ->
                                        False
                            )
                            model.requests

                    locations : List Location
                    locations =
                        model.locations
                            |> Dict.values
                            |> List.filter (\location -> location.store == store.id)
                            |> List.sortBy .position
                in
                if List.isEmpty locations then
                    text ""

                else
                    div
                        []
                        [ viewStorePageLocationTab locations
                        , case locations of
                            -- Can't reorder one location :)
                            [ _ ] ->
                                text ""

                            _ ->
                                -- Only allow reordering if we aren't deleting a location (to avoid weird states where
                                -- the delete ends up failing). There's probably a nicer way to do this than to omit
                                -- the button entirely.
                                if deletingLocation then
                                    text ""

                                else
                                    button
                                        [ class "fab-button reorder-button"
                                        , onClick (ReorderLocationsStart store.id)
                                        ]
                                        [ smallMutedSymbol "edit" ]
                        ]

      else
        viewStorePageItemTab model store
    ]


viewStorePageItemTab : Model -> Store -> Html Msg
viewStorePageItemTab model store =
    let
        viewItem : Item -> Html Msg
        viewItem item =
            let
                url =
                    pageUrl (StoreItemPage store.id item.id)

                chevron =
                    smallMutedSymbol "chevron_right"
            in
            li
                []
                [ case Dict.get item.id model.itemStores |> Maybe.andThen (Dict.get store.id) of
                    Just itemStore ->
                        if itemStore.sold then
                            Html.a
                                [ href url ]
                                [ span [] [ text item.name, chevron ]
                                , case itemStore.location of
                                    Nothing ->
                                        text ""

                                    Just location ->
                                        viewLocationLabel model location
                                ]

                        else
                            Html.a
                                [ class "muted"
                                , href url
                                ]
                                [ span [] [ span [ class "strikethrough" ] [ text item.name ], chevron ] ]

                    Nothing ->
                        Html.a
                            [ href url ]
                            [ span [] [ text (item.name ++ "?"), chevron ] ]
                ]
    in
    model.items
        |> Dict.values
        |> List.sortBy (.name >> String.toLower)
        |> List.map viewItem
        |> ul []


viewStorePageLocationTab : List Location -> Html Msg
viewStorePageLocationTab locations =
    let
        viewLocation : Location -> Html Msg
        viewLocation location =
            li
                []
                [ Html.a
                    [ href (pageUrl (LocationPage location.id)) ]
                    [ span
                        []
                        [ text location.name
                        , smallMutedSymbol "chevron_right"
                        ]
                    ]
                ]
    in
    ul [] (List.map viewLocation locations)


viewStorePageLocationTabReorder : Model -> Html Msg
viewStorePageLocationTabReorder model =
    let
        viewLocation : Int -> LocationId -> Html Msg
        viewLocation index locationId =
            li
                [ class "reorder-item"
                , onClick (ReorderLocationsMoveUp index)
                ]
                [ div []
                    [ text
                        (case Dict.get locationId model.locations of
                            Just location ->
                                location.name

                            Nothing ->
                                ""
                        )
                    ]
                , span [ class "material-symbols-outlined icon-sm muted padding-right-4px" ] [ text "arrow_upward" ]
                ]
    in
    ul [] (List.indexedMap viewLocation model.reorderingLocations)


{-| Drilling down into an item-then-store is very similar to store-then-item, so they share this helper.
-}
viewStoreAndItemPageHelper : Model -> Page -> Store -> Item -> List (Html Msg)
viewStoreAndItemPageHelper model backPage store item =
    let
        backUrl : String
        backUrl =
            pageUrl backPage

        maybeItemStore : Maybe ItemStore
        maybeItemStore =
            Dict.get item.id model.itemStores |> Maybe.andThen (Dict.get store.id)

        locations : List Location
        locations =
            List.sortBy (.name >> String.toLower) (getStoreLocations model store.id)

        viewLocation : Location -> Html Msg
        viewLocation location =
            let
                selected =
                    case maybeItemStore of
                        Nothing ->
                            False

                        Just itemStore ->
                            case itemStore.location of
                                Just location1 ->
                                    location1 == location.id

                                Nothing ->
                                    False
            in
            li
                [ class
                    (if selected then
                        "active"

                     else
                        ""
                    )
                , onClick
                    (if selected then
                        ReplaceUrl backUrl

                     else
                        ItemInStore item.id store.id (Just location.id)
                    )
                ]
                [ div [] [ text location.name ] ]
    in
    [ div
        [ class "page-header" ]
        [ Html.a
            [ href backUrl ]
            [ smallMutedSymbol "arrow_back" ]
        , h1 [] [ text (item.name ++ " at " ++ store.name) ]
        ]
    , ul
        []
        (List.map viewLocation locations
            ++ [ let
                    selected =
                        case maybeItemStore of
                            Nothing ->
                                False

                            Just itemStore ->
                                itemStore.sold && isNothing itemStore.location
                 in
                 li
                    [ class
                        (if selected then
                            "active"

                         else
                            ""
                        )
                    , onClick
                        (if selected then
                            ReplaceUrl backUrl

                         else
                            ItemInStore item.id store.id Nothing
                        )
                    ]
                    [ div []
                        [ text
                            (if List.isEmpty locations then
                                "Sold here"

                             else
                                "Somewhere"
                            )
                        ]
                    ]
               , let
                    selected =
                        case maybeItemStore of
                            Nothing ->
                                False

                            Just itemStore ->
                                not itemStore.sold
                 in
                 li
                    [ class
                        (if selected then
                            "active"

                         else
                            ""
                        )
                    , onClick
                        (if selected then
                            ReplaceUrl backUrl

                         else
                            ItemNotInStore item.id store.id
                        )
                    ]
                    [ div [] [ text "Not sold here" ] ]
               ]
        )
    ]


viewStoresPage : Model -> List (Html Msg)
viewStoresPage model =
    let
        viewStore : Store -> Html Msg
        viewStore store =
            li
                []
                [ Html.a
                    [ href (pageUrl (StorePage store.id)) ]
                    [ span
                        []
                        [ text store.name
                        , smallMutedSymbol "chevron_right"
                        ]
                    ]
                ]
    in
    [ div
        []
        [ div [ class "page-header" ] [ h1 [] [ text "Stores" ] ]
        , ul
            []
            (model.stores
                |> Dict.values
                |> List.sortBy (.name >> String.toLower)
                |> List.map viewStore
            )
        ]
    ]


viewLocationLabel : Model -> LocationId -> Html Msg
viewLocationLabel model locationId =
    case Dict.get locationId model.locations of
        Just location ->
            viewLocationLabel1 location

        Nothing ->
            text ""


viewLocationLabel1 : Location -> Html Msg
viewLocationLabel1 location =
    span [ class "location-label" ] [ text location.name ]


smallMutedSymbol : String -> Html msg
smallMutedSymbol name =
    span [ class "material-symbols-outlined icon-sm muted" ] [ text name ]


editNameInput : String -> Html Msg
editNameInput name =
    textarea
        [ autocomplete False
        , class "h1-edit"
        , id "name-edit"
        , onBlur EditNameSubmit
        , onFocus EditName
        , onInput EditNameChanged
        , preventDefaultOn "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "Enter" ->
                                Decode.succeed ( Blur "name-edit", True )

                            _ ->
                                Decode.fail ""
                    )
            )
        , rows 1
        , value name
        ]
        []



------------------------------------------------------------------------------------------------------------------------
-- HTTP requests and responses


type Request
    = RequestCreateItem CreateItemRequest
    | RequestCreateLocation CreateLocationRequest
    | RequestCreateStore CreateStoreRequest
    | RequestDeleteItem DeleteItemRequest
    | RequestDeleteLocation StoreId DeleteLocationRequest
    | RequestDeleteStore DeleteStoreRequest
    | RequestGetItems GetItemsRequest
    | RequestItemInStore ItemInStoreRequest
    | RequestItemNotInStore ItemNotInStoreRequest
    | RequestMoveItemOff MoveItemOffRequest
    | RequestMoveItemOn MoveItemOnRequest
    | RequestRenameItem RenameItemRequest
    | RequestRenameLocation RenameLocationRequest
    | RequestRenameStore RenameStoreRequest
    | RequestReorderLocations ReorderLocationsRequest


type alias CreateItemRequest =
    { name : String
    , onList : Bool
    , store : Maybe StoreId
    }


type alias CreateLocationRequest =
    { store : StoreId
    , name : String
    }


type alias CreateStoreRequest =
    { name : String
    }


type alias DeleteItemRequest =
    { id : ItemId
    }


type alias DeleteLocationRequest =
    { id : LocationId
    }


type alias DeleteStoreRequest =
    { id : StoreId
    }


type alias GetItemsRequest =
    { dataVersion : Int
    }


type alias ItemInStoreRequest =
    { item : ItemId
    , store : StoreId
    , location : Maybe LocationId
    }


type alias ItemNotInStoreRequest =
    { item : ItemId
    , store : StoreId
    }


type alias MoveItemOffRequest =
    { item : ItemId
    , store : Maybe StoreId
    , location : Maybe LocationId
    }


type alias MoveItemOnRequest =
    { item : ItemId
    , store : Maybe StoreId
    }


type alias ReorderLocationsRequest =
    { store : StoreId
    , locations : List LocationId
    }


type alias RenameItemRequest =
    { id : ItemId
    , name : String
    }


type alias RenameLocationRequest =
    { id : LocationId
    , store : StoreId
    , name : String
    }


type alias RenameStoreRequest =
    { id : StoreId
    , name : String
    }


{-| FIXME don't have to tuck locations into responses anymore because we have request queue
-}
type Response
    = ResponseCreateItem CreateItemRequest (Result Http.Error CreateItemResponse)
    | ResponseCreateLocation CreateLocationRequest (Result Http.Error CreateLocationResponse)
    | ResponseCreateStore CreateStoreRequest (Result Http.Error CreateStoreResponse)
    | ResponseDeleteItem DeleteItemRequest (Result Http.Error DeleteItemResponse)
    | ResponseDeleteLocation StoreId DeleteLocationRequest (Result Http.Error DeleteLocationResponse)
    | ResponseDeleteStore DeleteStoreRequest (Result Http.Error DeleteStoreResponse)
    | ResponseGetItems (Result Http.Error GetItemsResponse)
    | ResponseItemInStore ItemInStoreRequest (Result Http.Error ItemInStoreResponse)
    | ResponseItemNotInStore ItemNotInStoreRequest (Result Http.Error ItemNotInStoreResponse)
    | ResponseMoveItemOff MoveItemOffRequest (Result Http.Error MoveItemOffResponse)
    | ResponseMoveItemOn MoveItemOnRequest (Result Http.Error MoveItemOnResponse)
    | ResponseRenameItem RenameItemRequest (Result Http.Error RenameItemResponse)
    | ResponseRenameLocation RenameLocationRequest (Result Http.Error RenameLocationResponse)
    | ResponseRenameStore RenameStoreRequest (Result Http.Error RenameStoreResponse)
    | ResponseReorderLocations ReorderLocationsRequest (Result Http.Error ReorderLocationsResponse)


type alias CreateItemResponse =
    { dataVersion : Int
    , id : ItemId
    }


type alias CreateLocationResponse =
    { dataVersion : Int
    , id : LocationId
    , position : Int
    }


type alias CreateStoreResponse =
    { dataVersion : Int
    , id : StoreId
    }


type alias DeleteItemResponse =
    { dataVersion : Int
    }


type alias DeleteLocationResponse =
    { dataVersion : Int
    }


type alias DeleteStoreResponse =
    { dataVersion : Int
    }


type alias GetItemsResponse =
    { dataVersion : Int
    , items : List Item
    , itemStores : List ItemStore
    , locations : List Location
    , stores : List Store
    }


type alias ItemInStoreResponse =
    { dataVersion : Int
    }


type alias ItemNotInStoreResponse =
    { dataVersion : Int
    }


type alias MoveItemOffResponse =
    { dataVersion : Int
    }


type alias MoveItemOnResponse =
    { dataVersion : Int
    }


type alias RenameItemResponse =
    { dataVersion : Int
    }


type alias RenameLocationResponse =
    { dataVersion : Int
    }


type alias RenameStoreResponse =
    { dataVersion : Int
    }


type alias ReorderLocationsResponse =
    { dataVersion : Int
    }


sendRequest : Request -> Cmd Msg
sendRequest request =
    case request of
        RequestCreateItem request1 ->
            sendCreateItemRequest request1

        RequestCreateLocation request1 ->
            sendCreateLocationRequest request1

        RequestCreateStore request1 ->
            sendCreateStoreRequest request1

        RequestDeleteItem request1 ->
            sendDeleteItemRequest request1

        RequestDeleteLocation storeId request1 ->
            sendDeleteLocationRequest storeId request1

        RequestDeleteStore request1 ->
            sendDeleteStoreRequest request1

        RequestGetItems dataVersion ->
            sendGetItemsRequest dataVersion

        RequestItemInStore request1 ->
            sendItemInStoreRequest request1

        RequestItemNotInStore request1 ->
            sendItemNotInStoreRequest request1

        RequestMoveItemOff request1 ->
            sendMoveItemOffRequest request1

        RequestMoveItemOn request1 ->
            sendMoveItemOnRequest request1

        RequestRenameItem request1 ->
            sendRenameItemRequest request1

        RequestRenameLocation request1 ->
            sendRenameLocationRequest request1

        RequestRenameStore request1 ->
            sendRenameStoreRequest request1

        RequestReorderLocations request1 ->
            sendReorderLocationsRequest request1


sendCreateItemRequest : CreateItemRequest -> Cmd Msg
sendCreateItemRequest request =
    let
        body =
            Encode.object
                [ ( "name", Encode.string request.name )
                , ( "on_list", Encode.bool request.onList )
                , ( "store"
                  , case request.store of
                        Nothing ->
                            Encode.null

                        Just store ->
                            Encode.int store
                  )
                ]
    in
    Http.post
        { url = "/api/create-item"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseCreateItem request)
                (Decode.map2
                    (\dataVersion id ->
                        { dataVersion = dataVersion
                        , id = id
                        }
                    )
                    (Decode.field "data_version" Decode.int)
                    (Decode.field "id" Decode.int)
                )
        }


sendCreateLocationRequest : CreateLocationRequest -> Cmd Msg
sendCreateLocationRequest request =
    let
        body =
            Encode.object
                [ ( "store", Encode.int request.store )
                , ( "name", Encode.string request.name )
                ]
    in
    Http.post
        { url = "/api/create-location"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseCreateLocation request)
                (Decode.map3
                    (\dataVersion id position ->
                        { dataVersion = dataVersion
                        , id = id
                        , position = position
                        }
                    )
                    (Decode.field "data_version" Decode.int)
                    (Decode.field "id" Decode.int)
                    (Decode.field "position" Decode.int)
                )
        }


sendCreateStoreRequest : CreateStoreRequest -> Cmd Msg
sendCreateStoreRequest request =
    let
        body =
            Encode.object
                [ ( "name", Encode.string request.name )
                ]
    in
    Http.post
        { url = "/api/create-store"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseCreateStore request)
                (Decode.map2
                    (\dataVersion id ->
                        { dataVersion = dataVersion
                        , id = id
                        }
                    )
                    (Decode.field "data_version" Decode.int)
                    (Decode.field "id" Decode.int)
                )
        }


sendDeleteItemRequest : DeleteItemRequest -> Cmd Msg
sendDeleteItemRequest request =
    let
        body =
            Encode.object
                [ ( "id", Encode.int request.id )
                ]
    in
    Http.post
        { url = "/api/delete-item"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseDeleteItem request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }


sendDeleteLocationRequest : StoreId -> DeleteLocationRequest -> Cmd Msg
sendDeleteLocationRequest storeId request =
    let
        body =
            Encode.object
                [ ( "id", Encode.int request.id )
                ]
    in
    Http.post
        { url = "/api/delete-location"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseDeleteLocation storeId request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }


sendDeleteStoreRequest : DeleteStoreRequest -> Cmd Msg
sendDeleteStoreRequest request =
    let
        body =
            Encode.object
                [ ( "id", Encode.int request.id )
                ]
    in
    Http.post
        { url = "/api/delete-store"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseDeleteStore request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }


sendGetItemsRequest : GetItemsRequest -> Cmd Msg
sendGetItemsRequest request =
    Http.request
        { method = "GET"
        , headers =
            if request.dataVersion > 0 then
                [ Http.header "If-None-Match" ("\"" ++ String.fromInt request.dataVersion ++ "\"") ]

            else
                []
        , url = "/api/items"
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (Response << ResponseGetItems)
                (Decode.map5
                    (\dataVersion items itemStores locations stores ->
                        { dataVersion = dataVersion
                        , items = items
                        , itemStores = itemStores
                        , locations = locations
                        , stores = stores
                        }
                    )
                    (Decode.field "data_version" Decode.int)
                    (Decode.field "items" (Decode.list itemDecoder))
                    (Decode.field "item_stores" (Decode.list itemStoreDecoder))
                    (Decode.field "locations" (Decode.list locationDecoder))
                    (Decode.field "stores" (Decode.list storeDecoder))
                )
        , timeout = Nothing
        , tracker = Nothing
        }


sendItemInStoreRequest : ItemInStoreRequest -> Cmd Msg
sendItemInStoreRequest request =
    let
        body =
            Encode.object
                [ ( "item", Encode.int request.item )
                , ( "store", Encode.int request.store )
                , ( "location"
                  , case request.location of
                        Just location ->
                            Encode.int location

                        Nothing ->
                            Encode.null
                  )
                ]
    in
    Http.post
        { url = "/api/item-in-store"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseItemInStore request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }


sendItemNotInStoreRequest : ItemNotInStoreRequest -> Cmd Msg
sendItemNotInStoreRequest request =
    let
        body =
            Encode.object
                [ ( "item", Encode.int request.item )
                , ( "store", Encode.int request.store )
                ]
    in
    Http.post
        { url = "/api/item-not-in-store"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseItemNotInStore request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }


sendMoveItemOffRequest : MoveItemOffRequest -> Cmd Msg
sendMoveItemOffRequest request =
    let
        body =
            Encode.object
                [ ( "item", Encode.int request.item )
                , ( "store"
                  , case request.store of
                        Just store ->
                            Encode.int store

                        Nothing ->
                            Encode.null
                  )
                , ( "location"
                  , case request.location of
                        Just location ->
                            Encode.int location

                        Nothing ->
                            Encode.null
                  )
                ]
    in
    Http.post
        { url = "/api/item-off"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseMoveItemOff request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }


sendMoveItemOnRequest : MoveItemOnRequest -> Cmd Msg
sendMoveItemOnRequest request =
    let
        body =
            Encode.object
                [ ( "item", Encode.int request.item )
                , ( "store"
                  , case request.store of
                        Nothing ->
                            Encode.null

                        Just store ->
                            Encode.int store
                  )
                ]
    in
    Http.post
        { url = "/api/item-on"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseMoveItemOn request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }


sendRenameItemRequest : RenameItemRequest -> Cmd Msg
sendRenameItemRequest request =
    let
        body =
            Encode.object
                [ ( "id", Encode.int request.id )
                , ( "name", Encode.string request.name )
                ]
    in
    Http.post
        { url = "/api/rename-item"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseRenameItem request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }


sendRenameLocationRequest : RenameLocationRequest -> Cmd Msg
sendRenameLocationRequest request =
    let
        body =
            Encode.object
                [ ( "id", Encode.int request.id )
                , ( "store", Encode.int request.store )
                , ( "name", Encode.string request.name )
                ]
    in
    Http.post
        { url = "/api/rename-location"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseRenameLocation request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }


sendReorderLocationsRequest : ReorderLocationsRequest -> Cmd Msg
sendReorderLocationsRequest request =
    let
        body : Value
        body =
            Encode.object
                [ ( "store", Encode.int request.store )
                , ( "locations", Encode.list Encode.int request.locations )
                ]
    in
    Http.post
        { url = "/api/reorder-locations"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseReorderLocations request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }


sendRenameStoreRequest : RenameStoreRequest -> Cmd Msg
sendRenameStoreRequest request =
    let
        body =
            Encode.object
                [ ( "id", Encode.int request.id )
                , ( "name", Encode.string request.name )
                ]
    in
    Http.post
        { url = "/api/rename-store"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseRenameStore request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }



------------------------------------------------------------------------------------------------------------------------
-- Model utility functions


assumeInFlightRequestsWillSucceed : Model -> Model
assumeInFlightRequestsWillSucceed =
    let
        go : Queue Request -> Model -> Model
        go requests model =
            case dequeue requests of
                Nothing ->
                    model

                Just ( request, requests1 ) ->
                    go requests1 (assumeRequestWillSucceed request model)
    in
    \model -> go model.requests model


assumeRequestWillSucceed : Request -> Model -> Model
assumeRequestWillSucceed request model =
    case request of
        RequestCreateItem _ ->
            model

        RequestCreateLocation _ ->
            model

        RequestCreateStore _ ->
            model

        RequestDeleteItem request1 ->
            assumeDeleteItemRequestWillSucceed request1 model

        RequestDeleteLocation storeId request1 ->
            assumeDeleteLocationRequestWillSucceed storeId request1 model

        RequestDeleteStore request1 ->
            assumeDeleteStoreRequestWillSucceed request1 model

        RequestGetItems _ ->
            model

        RequestItemInStore request1 ->
            assumeItemInStoreRequestWillSucceed request1 model

        RequestItemNotInStore request1 ->
            assumeItemNotInStoreRequestWillSucceed request1 model

        RequestMoveItemOff request1 ->
            assumeMoveItemOffRequestWillSucceed request1 model

        RequestMoveItemOn request1 ->
            assumeMoveItemOnRequestWillSucceed request1 model

        RequestRenameItem request1 ->
            assumeRenameItemRequestWillSucceed request1 model

        RequestRenameLocation request1 ->
            assumeRenameLocationRequestWillSucceed request1 model

        RequestRenameStore request1 ->
            assumeRenameStoreRequestWillSucceed request1 model

        RequestReorderLocations request1 ->
            assumeReorderLocationsRequestWillSucceed request1 model


assumeDeleteItemRequestWillSucceed : DeleteItemRequest -> Model -> Model
assumeDeleteItemRequestWillSucceed request model =
    let
        items =
            Dict.remove request.id model.items
    in
    { model
        | items = items
        , -- we could be more surgical but whatever, deleting items isn't common
          itemsIndex = buildItemsIndex (Dict.values items)
        , itemStores = Dict.remove request.id model.itemStores
    }


assumeDeleteLocationRequestWillSucceed : StoreId -> DeleteLocationRequest -> Model -> Model
assumeDeleteLocationRequestWillSucceed storeId request model =
    { model
        | itemStores =
            Dict.map
                (\_ ->
                    dictUpdateIfExists
                        storeId
                        (\itemStore ->
                            if itemStore.location == Just request.id then
                                { itemStore | location = Nothing }

                            else
                                itemStore
                        )
                )
                model.itemStores
        , locations = Dict.remove request.id model.locations
    }


assumeDeleteStoreRequestWillSucceed : DeleteStoreRequest -> Model -> Model
assumeDeleteStoreRequestWillSucceed request model =
    { model
        | itemStores = Dict.map (\_ itemStores -> Dict.remove request.id itemStores) model.itemStores
        , locations = Dict.filter (\_ location -> location.store /= request.id) model.locations
        , stores = Dict.remove request.id model.stores
    }


assumeItemInStoreRequestWillSucceed : ItemInStoreRequest -> Model -> Model
assumeItemInStoreRequestWillSucceed request model =
    { model
        | itemStores =
            dictUpsert
                request.item
                (Maybe.withDefault Dict.empty
                    >> Dict.insert
                        request.store
                        { item = request.item
                        , store = request.store
                        , sold = True
                        , location = request.location
                        }
                )
                model.itemStores
    }


assumeItemNotInStoreRequestWillSucceed : ItemNotInStoreRequest -> Model -> Model
assumeItemNotInStoreRequestWillSucceed request model =
    { model
        | itemStores =
            dictUpsert
                request.item
                (Maybe.withDefault Dict.empty
                    >> Dict.insert
                        request.store
                        { item = request.item
                        , store = request.store
                        , sold = False
                        , location = Nothing
                        }
                )
                model.itemStores
        , -- If the user clicked "Not sold here" after tapping into an item with a question mark while shopping,
          -- this will be Just, and setting it to Nothing takes them back
          shoppingPageLocationSelection = Nothing
    }


assumeMoveItemOffRequestWillSucceed : MoveItemOffRequest -> Model -> Model
assumeMoveItemOffRequestWillSucceed request model =
    { model
        | items =
            dictUpdateIfExists
                request.item
                (\item -> { item | onList = False })
                model.items
        , itemStores =
            case request.store of
                Nothing ->
                    model.itemStores

                Just storeId ->
                    dictUpsert
                        request.item
                        (Maybe.withDefault Dict.empty
                            >> dictUpsert
                                storeId
                                (\maybeItemStore ->
                                    case maybeItemStore of
                                        Nothing ->
                                            { item = request.item
                                            , store = storeId
                                            , sold = True
                                            , location = request.location
                                            }

                                        -- Additive information from request: it's definitely sold here (so
                                        -- possibly flip sold=False to sold=True), and also possibly increase
                                        -- known location about location (but don't go Just->Nothing)
                                        Just itemStore ->
                                            { itemStore
                                                | sold = True
                                                , location =
                                                    if isNothing request.location then
                                                        itemStore.location

                                                    else
                                                        request.location
                                            }
                                )
                        )
                        model.itemStores
        , listPageJustMovedOffItems =
            case model.page of
                ListPage ->
                    Set.insert request.item model.listPageJustMovedOffItems

                _ ->
                    model.listPageJustMovedOffItems
        , shoppingPageLocationSelection = Nothing
    }


assumeMoveItemOnRequestWillSucceed : MoveItemOnRequest -> Model -> Model
assumeMoveItemOnRequestWillSucceed request model =
    { model
        | items = dictUpdateIfExists request.item (\item -> { item | onList = True }) model.items
        , itemStores =
            case request.store of
                Nothing ->
                    model.itemStores

                Just store ->
                    dictUpsert
                        request.item
                        (Maybe.withDefault Dict.empty
                            >> dictUpsert
                                store
                                (\maybeItemStore ->
                                    case maybeItemStore of
                                        Nothing ->
                                            { item = request.item
                                            , store = store
                                            , sold = True
                                            , location = Nothing
                                            }

                                        Just itemStore ->
                                            { itemStore | sold = True }
                                )
                        )
                        model.itemStores
        , listPageJustMovedOffItems =
            case model.page of
                ListPage ->
                    Set.remove request.item model.listPageJustMovedOffItems

                _ ->
                    model.listPageJustMovedOffItems
    }


assumeRenameItemRequestWillSucceed : RenameItemRequest -> Model -> Model
assumeRenameItemRequestWillSucceed request model =
    let
        items =
            dictUpdateIfExists
                request.id
                (\item -> { item | name = request.name })
                model.items
    in
    { model
        | items = items
        , -- we could be more surgical but whatever, renaming items isn't common
          itemsIndex = buildItemsIndex (Dict.values items)
    }


assumeRenameLocationRequestWillSucceed : RenameLocationRequest -> Model -> Model
assumeRenameLocationRequestWillSucceed request model =
    { model
        | locations =
            dictUpdateIfExists
                request.id
                (\location -> { location | name = request.name })
                model.locations
    }


assumeRenameStoreRequestWillSucceed : RenameStoreRequest -> Model -> Model
assumeRenameStoreRequestWillSucceed request model =
    { model
        | stores =
            dictUpdateIfExists
                request.id
                (\store -> { store | name = request.name })
                model.stores
    }


assumeReorderLocationsRequestWillSucceed : ReorderLocationsRequest -> Model -> Model
assumeReorderLocationsRequestWillSucceed =
    -- Set locations within a model to be in the order of the given list (e.g. the first location is position 0, the
    -- second is position 1...). This is used both when sending a reorder request (optimistically assuming success) and
    -- also when receiving an error response to a reorder request (to roll back the assumption).
    let
        go position locationIds locations =
            case locationIds of
                locationId :: locationIds1 ->
                    go
                        (position + 1)
                        locationIds1
                        (dictUpdateIfExists
                            locationId
                            (\location -> { location | position = position })
                            locations
                        )

                [] ->
                    locations
    in
    \request model -> { model | locations = go 0 request.locations model.locations }


{-| Does this store have any locations?
-}
doesStoreHaveLocations : Model -> StoreId -> Bool
doesStoreHaveLocations model storeId =
    let
        loop : List Location -> Bool
        loop locations =
            case locations of
                location :: locations1 ->
                    if location.store == storeId then
                        True

                    else
                        loop locations1

                [] ->
                    False
    in
    loop (Dict.values model.locations)


dropRequest : Model -> Model
dropRequest model =
    { model | requests = queueDrop1 model.requests }


{-| Given a `data_version` from the server in response to a request that bumped it, if it appears a concurrent edit has
occurred (because the actual data version is more than 1 higher than the previous one), catch the client up by
enqueueing (to the front of the queue, before all other enqueued requests) a GET /items request.

Be careful to leave the model's dataVersion field alone if we're behind, so we send an old ETag along with our request
for the items.

-}
fetchItemsIfOutOfDate : Int -> Model -> Eff Model Msg
fetchItemsIfOutOfDate actualDataVersion model =
    if actualDataVersion == model.dataVersion + 1 then
        pure { model | dataVersion = actualDataVersion }

    else
        pure model
            |> andThen (enqueueRequestToFront (RequestGetItems { dataVersion = model.dataVersion }))


{-| Fire off the next request, if there is one.
-}
fireOffNextRequest : Model -> Eff Model Msg
fireOffNextRequest model =
    case queuePeek model.requests of
        Nothing ->
            ( model, Cmd.none )

        Just ( request, requests ) ->
            ( { model | requests = requests }
            , sendRequest request
            )


getItemByName : Model -> String -> Maybe Item
getItemByName model name =
    listFind (\item -> item.name == name) (Dict.values model.items)


{-| Get a store's locations (sorted arbitrarily)
-}
getStoreLocations : Model -> StoreId -> List Location
getStoreLocations model storeId =
    model.locations
        |> Dict.values
        |> List.filter (\location -> location.store == storeId)


{-| Is a particular item known to be sold at a particular store? Returns false if either we don't know, or no.
-}
isItemKnownToBeSoldAtStore : Model -> StoreId -> ItemId -> Bool
isItemKnownToBeSoldAtStore model storeId itemId =
    case Dict.get itemId model.itemStores |> Maybe.andThen (Dict.get storeId) of
        Nothing ->
            False

        Just itemStore ->
            itemStore.sold


enqueueRequest : Request -> Model -> Eff Model Msg
enqueueRequest request model =
    ( { model | requests = enqueue request model.requests }
    , -- If the queue was empty before enqueueing this request, send it.
      if queueIsEmpty model.requests then
        sendRequest request

      else
        Cmd.none
    )


enqueueRequestToFront : Request -> Model -> Eff Model Msg
enqueueRequestToFront request model =
    ( { model | requests = enqueueToFront request model.requests }
    , -- If the queue was empty before enqueueing this request, send it.
      if queueIsEmpty model.requests then
        sendRequest request

      else
        Cmd.none
    )



------------------------------------------------------------------------------------------------------------------------
-- Item type


type alias ItemId =
    Int


type alias Item =
    { id : ItemId
    , name : String
    , onList : Bool
    }


itemDecoder : Decoder Item
itemDecoder =
    Decode.map3
        Item
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "on_list" Decode.bool)



------------------------------------------------------------------------------------------------------------------------
-- ItemStore type


type alias ItemStore =
    { item : ItemId
    , store : StoreId
    , sold : Bool
    , location : Maybe LocationId
    }


itemStoreDecoder : Decoder ItemStore
itemStoreDecoder =
    Decode.map4
        ItemStore
        (Decode.field "item" Decode.int)
        (Decode.field "store" Decode.int)
        (Decode.field "sold" Decode.bool)
        (Decode.field "location" (Decode.nullable Decode.int))



------------------------------------------------------------------------------------------------------------------------
-- Location type


type alias LocationId =
    Int


type alias Location =
    { id : LocationId
    , store : StoreId
    , position : Int
    , name : String
    }


locationDecoder : Decoder Location
locationDecoder =
    Decode.map4
        Location
        (Decode.field "id" Decode.int)
        (Decode.field "store" Decode.int)
        (Decode.field "position" Decode.int)
        (Decode.field "name" Decode.string)



------------------------------------------------------------------------------------------------------------------------
-- Store type


type alias StoreId =
    Int


type alias Store =
    { id : StoreId
    , name : String
    }


storeDecoder : Decoder Store
storeDecoder =
    Decode.map2
        Store
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)



------------------------------------------------------------------------------------------------------------------------
-- Items index abstraction, for finding matching items by name


buildItemsIndex : List Item -> List ( String, Set ItemId )
buildItemsIndex =
    List.foldl
        (\item acc ->
            List.foldl
                (\term ->
                    dictUpsert
                        term
                        (Maybe.withDefault Set.empty >> Set.insert item.id)
                )
                acc
                (String.words (String.toLower item.name))
        )
        Dict.empty
        >> Dict.toList


searchItemsIndex : String -> List ( String, Set ItemId ) -> Set ItemId
searchItemsIndex query index =
    query
        |> String.toLower
        |> String.words
        |> List.map
            (\queryTerm ->
                List.foldl
                    (\( term, items ) acc ->
                        if String.startsWith queryTerm term then
                            Set.union items acc

                        else
                            acc
                    )
                    Set.empty
                    index
            )
        |> setIntersects



------------------------------------------------------------------------------------------------------------------------
-- Effectful value abstraction


type alias Eff model msg =
    ( model, Cmd msg )


purely : (model -> model) -> Eff model msg -> Eff model msg
purely =
    Tuple.mapFirst


andThen : (model -> Eff model msg) -> Eff model msg -> Eff model msg
andThen f ( model, cmd ) =
    let
        ( model1, cmd1 ) =
            f model
    in
    ( model1, Cmd.batch [ cmd, cmd1 ] )


andThenIf : Bool -> (model -> Eff model msg) -> Eff model msg -> Eff model msg
andThenIf b f x =
    if b then
        andThen f x

    else
        x


pure : model -> Eff model msg
pure model =
    ( model, Cmd.none )


command : Cmd msg -> Eff model msg -> Eff model msg
command cmd2 ( model, cmd1 ) =
    ( model, Cmd.batch [ cmd1, cmd2 ] )



------------------------------------------------------------------------------------------------------------------------
-- Queue type


type Queue a
    = Queue (List a) (List a)


enqueue : a -> Queue a -> Queue a
enqueue x (Queue xs ys) =
    Queue xs (x :: ys)


enqueueToFront : a -> Queue a -> Queue a
enqueueToFront x (Queue xs ys) =
    Queue (x :: xs) ys


dequeue : Queue a -> Maybe ( a, Queue a )
dequeue (Queue xs ys) =
    case xs of
        x :: xs1 ->
            Just ( x, Queue xs1 ys )

        [] ->
            case List.reverse ys of
                y :: ys1 ->
                    Just ( y, Queue ys1 [] )

                [] ->
                    Nothing


queueContains : (a -> Bool) -> Queue a -> Bool
queueContains p (Queue xs ys) =
    List.any p xs || List.any p ys


queueDrop1 : Queue a -> Queue a
queueDrop1 (Queue xs ys) =
    case xs of
        _ :: xs1 ->
            Queue xs1 ys

        [] ->
            case List.reverse ys of
                _ :: ys1 ->
                    Queue ys1 []

                [] ->
                    Queue [] []


queueIsEmpty : Queue a -> Bool
queueIsEmpty queue =
    case queue of
        Queue [] [] ->
            True

        _ ->
            False


queuePeek : Queue a -> Maybe ( a, Queue a )
queuePeek ((Queue xs ys) as queue) =
    case xs of
        x :: _ ->
            Just ( x, queue )

        [] ->
            case List.reverse ys of
                y :: ys1 ->
                    Just ( y, Queue ys1 [] )

                [] ->
                    Nothing



------------------------------------------------------------------------------------------------------------------------
-- Random utils


dictUpdateIfExists : comparable -> (v -> v) -> Dict comparable v -> Dict comparable v
dictUpdateIfExists k f =
    Dict.update k (Maybe.map f)


dictUpsert : comparable -> (Maybe v -> v) -> Dict comparable v -> Dict comparable v
dictUpsert k f =
    Dict.update k (f >> Just)


isNothing : Maybe a -> Bool
isNothing x =
    case x of
        Nothing ->
            True

        Just _ ->
            False


listFind : (a -> Bool) -> List a -> Maybe a
listFind p xs =
    case xs of
        y :: ys ->
            if p y then
                Just y

            else
                listFind p ys

        [] ->
            Nothing


{-| Get the intersection of a list of sets.
-}
setIntersects : List (Set comparable) -> Set comparable
setIntersects xs =
    case xs of
        x :: xs1 ->
            List.foldl Set.intersect x xs1

        [] ->
            Set.empty
