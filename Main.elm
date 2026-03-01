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
import Html.Events exposing (onBlur, onFocus, onInput, preventDefaultOn)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Process
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
    , initializing : Bool
    , itemStores : Dict ItemId (Dict StoreId ItemStore)
    , items : Dict ItemId Item
    , -- An items search index. Must be manually kept in-sync with items.
      itemsIndex : List ( String, Set ItemId )
    , key : Navigation.Key
    , -- On the list page, the set of items that were on the list when we tapped them (moving them off it)
      -- We remember these in a set so items don't jump when you move them off the list. This makes it easier to add
      -- back something that was accidentally removed.
      listPageJustMovedOffItems : Set ItemId
    , longpressed : Bool
    , page : Page
    , reorderingSections : List SectionId
    , -- Outstanding HTTP requests. The front of the queue is in-flight.
      requests : Queue Request
    , sections : Dict SectionId Section
    , shopping : Maybe StoreId
    , storePageTab : Bool -- haha terrible bool. True=Sections, False=Items
    , stores : Dict StoreId Store
    , tapCount : Int
    }


type Page
    = ListPage
    | ItemPage ItemId
    | ItemStorePage ItemId StoreId
    | ShoppingItemPage StoreId ItemId -- short-pressed an item, have location info to fill in
    | ShoppingPage StoreId
    | ShoppingSelectionPage
    | StoreItemPage StoreId ItemId
    | StorePage StoreId
    | StoreSectionItemPage StoreId SectionId ItemId
    | StoreSectionPage StoreId SectionId
    | StoresPage


type Msg
    = AddItemIdToList ItemId
    | AddItemNameToList String
    | AddItemNameToStore StoreId String
    | ClearLongpressed
    | CreateSection StoreId String
    | CreateStore (Maybe ItemId) String
    | Blur String
    | ClickedBack
    | ConfirmDelete
    | CreateItem String
    | DocumentBecameVisible
    | DoneShopping
    | DeleteItem ItemId
    | DeleteSection Section
    | DeleteStore StoreId
    | DismissError
    | EditName
    | EditNameChanged String
    | EditNameSubmit
    | FabClose
    | FabOpen
    | FabValueChanged String
    | ItemInStore ItemId StoreId (Maybe SectionId)
    | ItemNotInStore ItemId StoreId
    | LongpressItem Int ItemId
    | PointercancelItem
    | PointerdownItem ItemId
    | PointerupItem ItemId
    | PointerupSection (Maybe (Maybe SectionId)) -- Nothing = Not sold here, Just Nothing = Sold here / Somewhere
    | Noop
    | ReorderSectionsDone StoreId
    | ReorderSectionsMoveUp Int
    | ReorderSectionsStart StoreId
    | Response Response
    | SetStorePageTab Bool
    | Shop
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
        , Url.Parser.map ShoppingItemPage (Url.Parser.s "shop" </> Url.Parser.int </> Url.Parser.s "item" </> Url.Parser.int)
        , Url.Parser.map ShoppingPage (Url.Parser.s "shop" </> Url.Parser.int)
        , Url.Parser.map ShoppingSelectionPage (Url.Parser.s "shop")
        , Url.Parser.map StoreItemPage (Url.Parser.s "store" </> Url.Parser.int </> Url.Parser.s "item" </> Url.Parser.int)
        , Url.Parser.map StorePage (Url.Parser.s "store" </> Url.Parser.int)
        , Url.Parser.map StoreSectionPage (Url.Parser.s "store" </> Url.Parser.int </> Url.Parser.s "section" </> Url.Parser.int)
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

        ShoppingItemPage storeId itemId ->
            "/shop/" ++ String.fromInt storeId ++ "/item/" ++ String.fromInt itemId

        ShoppingPage storeId ->
            "/shop/" ++ String.fromInt storeId

        ShoppingSelectionPage ->
            "/shop"

        StoreItemPage storeId itemId ->
            "/store/" ++ String.fromInt storeId ++ "/item/" ++ String.fromInt itemId

        StoreSectionPage storeId sectionId ->
            "/store/" ++ String.fromInt storeId ++ "/section/" ++ String.fromInt sectionId

        StoreSectionItemPage storeId sectionId itemId ->
            "/store/" ++ String.fromInt storeId ++ "/section/" ++ String.fromInt sectionId ++ "/item/" ++ String.fromInt itemId

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
            , initializing = True
            , items = Dict.empty
            , itemsIndex = []
            , itemStores = Dict.empty
            , key = key
            , listPageJustMovedOffItems = Set.empty
            , longpressed = False
            , page = page
            , reorderingSections = []
            , requests = Queue [] []
            , sections = Dict.empty
            , shopping =
                case page of
                    ShoppingPage storeId ->
                        Just storeId

                    _ ->
                        Nothing
            , storePageTab = True
            , stores = Dict.empty
            , tapCount = 0
            }
    in
    case Url.Parser.parse pageParser url of
        Just page ->
            pure (model page)
                |> andThen (enqueueRequest (RequestGetItems { dataVersion = dataVersion }))

        -- FIXME should probably serve some kind of "I'm busted" page
        Nothing ->
            pure (model ListPage)


update : Msg -> Model -> Eff Model Msg
update msg model =
    case msg of
        AddItemIdToList itemId ->
            handleAddItemIdToList model itemId

        AddItemNameToList name ->
            handleAddItemNameToList model name

        AddItemNameToStore storeId name ->
            handleAddItemNameToStore model storeId name

        Blur id ->
            handleBlur model id

        ClearLongpressed ->
            handleClearLongpressed model

        ClickedBack ->
            handleClickedBack model

        ConfirmDelete ->
            handleConfirmDelete model

        CreateItem name ->
            handleCreateItem model name

        CreateSection storeId name ->
            handleCreateSection model storeId name

        CreateStore item name ->
            handleCreateStore model item name

        DocumentBecameVisible ->
            handleDocumentBecameVisible model

        DoneShopping ->
            handleDoneShopping model

        DeleteItem id ->
            handleDeleteItem model id

        DeleteSection section ->
            handleDeleteSection model section

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

        ItemInStore itemId storeId maybeSectionId ->
            handleItemInStore model itemId storeId maybeSectionId

        ItemNotInStore itemId storeId ->
            handleItemNotInStore model itemId storeId

        LongpressItem tapCount itemId ->
            handleLongpressItem model tapCount itemId

        Noop ->
            pure model

        PointercancelItem ->
            handlePointercancelItem model

        PointerdownItem itemId ->
            handlePointerdownItem model itemId

        PointerupItem itemId ->
            handlePointerupItem model itemId

        PointerupSection sectionId ->
            handlePointerupSection model sectionId

        ReorderSectionsDone storeId ->
            handleReorderSectionsDone model storeId

        ReorderSectionsMoveUp index ->
            handleReorderSectionsMoveUp model index

        ReorderSectionsStart storeId ->
            handleReorderSectionsStart model storeId

        Response response ->
            handleResponse model response

        SetStorePageTab tab ->
            handleSetStorePageTab model tab

        Shop ->
            handleShop model

        ToggleDarkMode ->
            handleToggleDarkMode model

        UrlChanged url ->
            handleUrlChanged model url

        UrlRequested urlRequest ->
            handleUrlRequested model urlRequest


handleAddItemIdToList : Model -> ItemId -> Eff Model Msg
handleAddItemIdToList model itemId =
    let
        model1 =
            { model | fabValue = Nothing }

        effectiveModel =
            assumeInFlightRequestsWillSucceed model1
    in
    case Dict.get itemId effectiveModel.items of
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

        Nothing ->
            pure model1


{-| Add an item to the list by name. It might already exist, in which case we'll just move it onto the list. Otherwise,
we'll create it.
-}
handleAddItemNameToList : Model -> String -> Eff Model Msg
handleAddItemNameToList model name =
    let
        name1 =
            String.trim name

        model1 =
            { model | fabValue = Nothing }
    in
    if String.isEmpty name1 then
        pure model1

    else
        let
            effectiveModel =
                assumeInFlightRequestsWillSucceed model
        in
        case getItemByName effectiveModel name1 of
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


handleAddItemNameToStore : Model -> StoreId -> String -> Eff Model Msg
handleAddItemNameToStore model store name =
    let
        name1 =
            String.trim name

        model1 =
            { model | fabValue = Nothing }
    in
    if String.isEmpty name1 then
        pure model1

    else
        let
            effectiveModel =
                assumeInFlightRequestsWillSucceed model
        in
        case getItemByName effectiveModel name1 of
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
                                    , section = Nothing
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
    let
        effectiveModel =
            assumeInFlightRequestsWillSucceed model
    in
    pure { model | fabValue = Nothing }
        |> andThenIf
            (not (String.isEmpty name1) && isNothing (getItemByName effectiveModel name1))
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


handleCreateSection : Model -> StoreId -> String -> Eff Model Msg
handleCreateSection model storeId name =
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
                    (RequestCreateSection
                        { store = storeId
                        , name = name1
                        }
                    )
                    model1
            )


handleCreateStore : Model -> Maybe ItemId -> String -> Eff Model Msg
handleCreateStore model maybeItem name =
    let
        name1 : String
        name1 =
            String.trim name
    in
    pure { model | fabValue = Nothing }
        |> andThenIf
            (not (String.isEmpty name1))
            (\model1 ->
                let
                    effectiveModel =
                        assumeInFlightRequestsWillSucceed model1
                in
                case getStoreByName effectiveModel name1 of
                    Nothing ->
                        enqueueRequest
                            (RequestCreateStore
                                { name = name1
                                , item = maybeItem
                                }
                            )
                            model1

                    Just store ->
                        case maybeItem of
                            Nothing ->
                                pure model1

                            Just item ->
                                if
                                    case Dict.get item effectiveModel.itemStores of
                                        Nothing ->
                                            True

                                        Just itemStores ->
                                            case Dict.get store.id itemStores of
                                                Nothing ->
                                                    True

                                                Just itemStore ->
                                                    not itemStore.sold
                                then
                                    enqueueRequest
                                        (RequestItemInStore
                                            { item = item
                                            , store = store.id
                                            , section = Nothing
                                            }
                                        )
                                        model1

                                else
                                    pure model1
            )


handleBlur : Model -> String -> Eff Model Msg
handleBlur model id =
    pure model
        |> command (Task.attempt (\_ -> Noop) (Browser.Dom.blur id))


handleClearLongpressed : Model -> Eff Model Msg
handleClearLongpressed model =
    pure { model | longpressed = False }


handleClickedBack : Model -> Eff Model Msg
handleClickedBack model =
    case model.page of
        ItemPage _ ->
            pure model
                |> replacePage
                    (case model.shopping of
                        Nothing ->
                            ListPage

                        Just storeId ->
                            ShoppingPage storeId
                    )

        ItemStorePage item _ ->
            pure model
                |> replacePage (ItemPage item)

        ShoppingItemPage store _ ->
            pure model
                |> replacePage (ShoppingPage store)

        StoreItemPage store _ ->
            pure model
                |> replacePage (StorePage store)

        StoreSectionPage store _ ->
            pure model
                |> replacePage (StorePage store)

        StoreSectionItemPage store section _ ->
            pure model
                |> replacePage (StoreSectionPage store section)

        StorePage _ ->
            pure model
                |> replacePage StoresPage

        _ ->
            pure model


handleConfirmDelete : Model -> Eff Model msg
handleConfirmDelete model =
    pure { model | confirmingDelete = True }


handleDocumentBecameVisible : Model -> Eff Model Msg
handleDocumentBecameVisible model =
    let
        getItemsRequestAlreadyEnqueued : Bool
        getItemsRequestAlreadyEnqueued =
            queueContains
                (\request ->
                    case request of
                        RequestGetItems _ ->
                            True

                        _ ->
                            False
                )
                model.requests
    in
    pure model
        |> andThenIf
            (not getItemsRequestAlreadyEnqueued)
            (\model1 -> enqueueRequest (RequestGetItems { dataVersion = model.dataVersion }) model1)


handleDoneShopping : Model -> Eff Model Msg
handleDoneShopping model =
    pure { model | shopping = Nothing }
        |> replacePage ListPage


handleDeleteItem : Model -> ItemId -> Eff Model Msg
handleDeleteItem model itemId =
    pure { model | confirmingDelete = False }
        |> andThen
            (\model1 ->
                let
                    effectiveModel =
                        assumeInFlightRequestsWillSucceed model1
                in
                case Dict.get itemId effectiveModel.items of
                    Just _ ->
                        enqueueRequest (RequestDeleteItem { id = itemId }) model1

                    Nothing ->
                        pure model1
            )
        |> replacePage ListPage


handleDeleteSection : Model -> Section -> Eff Model Msg
handleDeleteSection model section =
    pure { model | confirmingDelete = False }
        |> andThen
            (\model1 ->
                let
                    effectiveModel =
                        assumeInFlightRequestsWillSucceed model1
                in
                case Dict.get section.id effectiveModel.sections of
                    Just _ ->
                        enqueueRequest (RequestDeleteSection section.store { id = section.id }) model1

                    Nothing ->
                        pure model1
            )
        |> replacePage (StorePage section.store)


handleDeleteStore : Model -> StoreId -> Eff Model Msg
handleDeleteStore model id =
    pure { model | confirmingDelete = False }
        |> andThen
            (\model1 ->
                let
                    effectiveModel =
                        assumeInFlightRequestsWillSucceed model1
                in
                case Dict.get id effectiveModel.stores of
                    Just _ ->
                        enqueueRequest (RequestDeleteStore { id = id }) model1

                    Nothing ->
                        pure model1
            )
        |> replacePage StoresPage


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

                    StorePage storeId ->
                        Maybe.map .name (Dict.get storeId model.stores)

                    StoreSectionPage _ sectionId ->
                        Maybe.map .name (Dict.get sectionId model.sections)

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
                let
                    effectiveModel =
                        assumeInFlightRequestsWillSucceed model
                in
                case Dict.get itemId effectiveModel.items of
                    Just item ->
                        pure { model | editingName = Nothing }
                            |> andThenIf
                                (newName1 /= item.name)
                                (\model1 ->
                                    enqueueRequest
                                        (RequestRenameItem
                                            { id = itemId
                                            , name = newName1
                                            }
                                        )
                                        model1
                                )

                    Nothing ->
                        pure model

        ( StoreSectionPage _ sectionId, Just newName ) ->
            let
                newName1 : String
                newName1 =
                    String.trim newName
            in
            if String.isEmpty newName1 then
                pure { model | editingName = Nothing }

            else
                let
                    effectiveModel =
                        assumeInFlightRequestsWillSucceed model
                in
                case Dict.get sectionId effectiveModel.sections of
                    Just section ->
                        pure
                            { model | editingName = Nothing }
                            |> andThenIf
                                (newName1 /= section.name)
                                (\model1 ->
                                    enqueueRequest
                                        (RequestRenameSection
                                            { id = sectionId
                                            , store = section.store
                                            , name = newName1
                                            }
                                        )
                                        model1
                                )

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
                let
                    effectiveModel =
                        assumeInFlightRequestsWillSucceed model
                in
                case Dict.get storeId effectiveModel.stores of
                    Just store ->
                        pure { model | editingName = Nothing }
                            |> andThenIf
                                (newName1 /= store.name)
                                (\model1 ->
                                    enqueueRequest
                                        (RequestRenameStore
                                            { id = storeId
                                            , name = newName1
                                            }
                                        )
                                        model1
                                )

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


handleItemInStore : Model -> ItemId -> StoreId -> Maybe SectionId -> Eff Model Msg
handleItemInStore model itemId storeId maybeSectionId =
    let
        effectiveModel =
            assumeInFlightRequestsWillSucceed model

        shouldSend =
            case Dict.get itemId effectiveModel.itemStores of
                Nothing ->
                    True

                Just itemStores ->
                    case Dict.get storeId itemStores of
                        Nothing ->
                            True

                        Just itemStore ->
                            not itemStore.sold || (itemStore.section /= maybeSectionId)
    in
    -- close fab because this could come from item page fab
    pure { model | fabValue = Nothing }
        |> andThenIf
            shouldSend
            (\model1 ->
                enqueueRequest
                    (RequestItemInStore
                        { item = itemId
                        , store = storeId
                        , section = maybeSectionId
                        }
                    )
                    model1
            )


handleItemNotInStore : Model -> ItemId -> StoreId -> Eff Model Msg
handleItemNotInStore model itemId storeId =
    let
        effectiveModel =
            assumeInFlightRequestsWillSucceed model

        shouldSend =
            case Dict.get itemId effectiveModel.itemStores of
                Nothing ->
                    True

                Just itemStores ->
                    case Dict.get storeId itemStores of
                        Nothing ->
                            True

                        Just itemStore ->
                            itemStore.sold
    in
    pure model
        |> andThenIf
            shouldSend
            (\model1 ->
                enqueueRequest
                    (RequestItemNotInStore
                        { item = itemId
                        , store = storeId
                        }
                    )
                    model1
            )


handleLongpressItem : Model -> Int -> ItemId -> Eff Model Msg
handleLongpressItem model tapCount itemId =
    if tapCount == model.tapCount then
        case model.page of
            ListPage ->
                pure { model | longpressed = True }
                    |> pushPage (ItemPage itemId)

            ShoppingPage storeId ->
                pure { model | longpressed = True }
                    |> pushPage (ItemStorePage itemId storeId)

            _ ->
                pure model

    else
        pure model


handlePointercancelItem : Model -> Eff Model Msg
handlePointercancelItem model =
    pure { model | tapCount = model.tapCount + 1 }


handlePointerdownItem : Model -> ItemId -> Eff Model Msg
handlePointerdownItem model itemId =
    pure model
        |> command (Process.sleep 500 |> Task.perform (\_ -> LongpressItem model.tapCount itemId))


handlePointerupItem : Model -> ItemId -> Eff Model Msg
handlePointerupItem model itemId =
    let
        model1 =
            { model
                | longpressed = False
                , tapCount = model.tapCount + 1
            }
    in
    case model1.page of
        ListPage ->
            let
                effectiveModel =
                    assumeInFlightRequestsWillSucceed model1
            in
            case Dict.get itemId effectiveModel.items of
                Just item ->
                    pure model1
                        |> andThen
                            (enqueueRequest
                                (if item.onList then
                                    RequestMoveItemOff { item = itemId }

                                 else
                                    RequestMoveItemOn
                                        { item = itemId
                                        , store = Nothing
                                        }
                                )
                            )

                Nothing ->
                    pure model1

        ShoppingPage storeId ->
            let
                effectiveModel =
                    assumeInFlightRequestsWillSucceed model1
            in
            case Dict.get itemId effectiveModel.itemStores of
                Just itemStores ->
                    case Dict.get storeId itemStores of
                        Just itemStore ->
                            if itemStore.sold then
                                pure model1
                                    |> andThen (enqueueRequest (RequestMoveItemOff { item = itemId }))

                            else
                                pure model1

                        Nothing ->
                            pure model1
                                |> pushPage (ShoppingItemPage storeId itemId)

                Nothing ->
                    pure model1
                        |> pushPage (ShoppingItemPage storeId itemId)

        _ ->
            pure model1


handlePointerupSection : Model -> Maybe (Maybe SectionId) -> Eff Model Msg
handlePointerupSection model maybeMaybeSectionId =
    -- FIXME ItemInStore/ItemNotInStore barely used, but we just call other handler for now
    case model.page of
        ItemStorePage itemId storeId ->
            case maybeMaybeSectionId of
                Just maybeSectionId ->
                    handleItemInStore model itemId storeId maybeSectionId

                Nothing ->
                    handleItemNotInStore model itemId storeId

        ShoppingItemPage storeId itemId ->
            (case maybeMaybeSectionId of
                Just maybeSectionId ->
                    handleItemInStore model itemId storeId maybeSectionId
                        |> andThen (enqueueRequest (RequestMoveItemOff { item = itemId }))

                Nothing ->
                    handleItemNotInStore model itemId storeId
            )
                |> replacePage (ShoppingPage storeId)

        StoreItemPage storeId itemId ->
            case maybeMaybeSectionId of
                Just maybeSectionId ->
                    handleItemInStore model itemId storeId maybeSectionId

                Nothing ->
                    handleItemNotInStore model itemId storeId

        _ ->
            pure model


handleResponse : Model -> Response -> Eff Model Msg
handleResponse model response =
    case dequeue model.requests of
        Just ( request, requests ) ->
            let
                model1 =
                    { model | requests = requests }
            in
            (case ( request, response ) of
                ( RequestCreateItem request1, ResponseCreateItem _ result ) ->
                    handleResponseCreateItem model1 request1 result

                ( RequestCreateSection request1, ResponseCreateSection _ result ) ->
                    handleResponseCreateSection model1 request1 result

                ( RequestCreateStore request1, ResponseCreateStore _ result ) ->
                    handleResponseCreateStore model1 request1 result

                ( RequestDeleteItem request1, ResponseDeleteItem _ result ) ->
                    handleResponseDeleteItem model1 request1 result

                ( RequestDeleteSection storeId request1, ResponseDeleteSection _ _ result ) ->
                    handleResponseDeleteSection model1 storeId request1 result

                ( RequestDeleteStore request1, ResponseDeleteStore _ result ) ->
                    handleResponseDeleteStore model1 request1 result

                ( RequestGetItems _, ResponseGetItems result ) ->
                    handleResponseGetItems model1 result

                ( RequestItemInStore request1, ResponseItemInStore _ result ) ->
                    handleResponseItemInStore model1 request1 result

                ( RequestItemNotInStore request1, ResponseItemNotInStore _ result ) ->
                    handleResponseItemNotInStore model1 request1 result

                ( RequestMoveItemOff request1, ResponseMoveItemOff _ result ) ->
                    handleResponseMoveItemOff model1 request1 result

                ( RequestMoveItemOn request1, ResponseMoveItemOn _ result ) ->
                    handleResponseMoveItemOn model1 request1 result

                ( RequestRenameItem request1, ResponseRenameItem _ result ) ->
                    handleResponseRenameItem model1 request1 result

                ( RequestRenameSection request1, ResponseRenameSection _ result ) ->
                    handleResponseRenameSection model1 request1 result

                ( RequestRenameStore request1, ResponseRenameStore _ result ) ->
                    handleResponseRenameStore model1 request1 result

                ( RequestReorderSections request1, ResponseReorderSections _ result ) ->
                    handleResponseReorderSections model1 request1 result

                _ ->
                    pure model1
            )
                |> andThen fireOffNextRequest

        Nothing ->
            pure model


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
                                        , section = Nothing
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
                    | itemStores =
                        case request.item of
                            Nothing ->
                                model.itemStores

                            Just item ->
                                dictUpsert
                                    item
                                    (Maybe.withDefault Dict.empty
                                        >> Dict.insert
                                            response.id
                                            { item = item
                                            , store = response.id
                                            , sold = True
                                            , section = Nothing
                                            }
                                    )
                                    model.itemStores
                    , stores =
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


handleResponseCreateSection :
    Model
    -> CreateSectionRequest
    -> Result Http.Error CreateSectionResponse
    -> Eff Model Msg
handleResponseCreateSection model request result =
    case result of
        Ok response ->
            pure
                { model
                    | sections =
                        Dict.insert
                            response.id
                            { id = response.id
                            , store = request.store
                            , position = response.position
                            , name = request.name
                            }
                            model.sections
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


handleResponseDeleteSection :
    Model
    -> StoreId
    -> DeleteSectionRequest
    -> Result Http.Error DeleteSectionResponse
    -> Eff Model Msg
handleResponseDeleteSection model storeId request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeDeleteSectionRequestWillSucceed storeId request)
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

                stores =
                    List.foldl
                        (\store acc -> Dict.insert store.id store acc)
                        Dict.empty
                        response.stores
            in
            pure
                { model
                    | dataVersion = response.dataVersion
                    , initializing = False
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
                    , sections =
                        List.foldl
                            (\section acc -> Dict.insert section.id section acc)
                            Dict.empty
                            response.sections
                    , stores = stores
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


handleResponseRenameSection :
    Model
    -> RenameSectionRequest
    -> Result Http.Error RenameSectionResponse
    -> Eff Model Msg
handleResponseRenameSection model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeRenameSectionRequestWillSucceed request)
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


handleResponseReorderSections :
    Model
    -> ReorderSectionsRequest
    -> Result Http.Error ReorderSectionsResponse
    -> Eff Model Msg
handleResponseReorderSections model request result =
    case result of
        Ok response ->
            pure model
                |> purely (assumeReorderSectionsRequestWillSucceed request)
                |> andThen (fetchItemsIfOutOfDate response.dataVersion)

        Err _ ->
            pure { model | error = Just "Something went wrong." }


handleReorderSectionsDone : Model -> StoreId -> Eff Model Msg
handleReorderSectionsDone model storeId =
    case model.reorderingSections of
        _ :: _ :: _ ->
            let
                effectiveModel =
                    assumeInFlightRequestsWillSucceed model

                currentSections =
                    getStoreSections effectiveModel storeId |> List.sortBy .position |> List.map .id
            in
            if model.reorderingSections /= currentSections then
                pure { model | reorderingSections = [] }
                    |> andThen
                        (enqueueRequest
                            (RequestReorderSections
                                { store = storeId
                                , sections = model.reorderingSections
                                }
                            )
                        )

            else
                pure { model | reorderingSections = [] }

        _ ->
            pure model


handleReorderSectionsMoveUp : Model -> Int -> Eff Model msg
handleReorderSectionsMoveUp model index =
    pure
        { model
            | reorderingSections =
                if index == 0 then
                    case model.reorderingSections of
                        section :: sections ->
                            sections ++ [ section ]

                        -- impossible
                        [] ->
                            model.reorderingSections

                else
                    case List.drop (index - 1) model.reorderingSections of
                        section1 :: section2 :: sections ->
                            List.take (index - 1) model.reorderingSections ++ (section2 :: section1 :: sections)

                        -- impossible
                        _ ->
                            model.reorderingSections
        }


handleReorderSectionsStart : Model -> StoreId -> Eff Model msg
handleReorderSectionsStart model storeId =
    let
        effectiveModel =
            assumeInFlightRequestsWillSucceed model
    in
    pure
        { model
            | reorderingSections =
                getStoreSections effectiveModel storeId
                    |> List.sortBy .position
                    |> List.map .id
        }


handleSetStorePageTab : Model -> Bool -> Eff Model msg
handleSetStorePageTab model tab =
    pure
        { model
            | reorderingSections = []
            , storePageTab = tab
        }


handleShop : Model -> Eff Model Msg
handleShop model =
    pure model
        |> replacePage
            (case model.shopping of
                Nothing ->
                    ShoppingSelectionPage

                Just storeId ->
                    ShoppingPage storeId
            )


handleToggleDarkMode : Model -> Eff Model Msg
handleToggleDarkMode model =
    pure { model | darkMode = not model.darkMode }
        |> command (saveModel (Encode.object [ ( "dark_mode", Encode.bool (not model.darkMode) ) ]))


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
                    , reorderingSections = []
                    , shopping =
                        case page of
                            ShoppingPage storeId ->
                                Just storeId

                            _ ->
                                model.shopping
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
                    pure model
                        |> (case page of
                                ListPage ->
                                    replacePage page

                                ItemPage _ ->
                                    pushPage page

                                ItemStorePage _ _ ->
                                    pushPage page

                                ShoppingItemPage _ _ ->
                                    pushPage page

                                ShoppingPage _ ->
                                    pushPage page

                                ShoppingSelectionPage ->
                                    replacePage page

                                StorePage _ ->
                                    pushPage page

                                StoreItemPage _ _ ->
                                    pushPage page

                                StoreSectionPage _ _ ->
                                    pushPage page

                                StoreSectionItemPage _ _ _ ->
                                    pushPage page

                                StoresPage ->
                                    replacePage page
                           )

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
        [ class (ifte model.darkMode "layout dark" "layout") ]
        [ -- If we just longpressed something, render a fullscreen thing to swallow the next pointerup event
          if model.longpressed then
            div
                [ class "screen"
                , onPointerup ClearLongpressed
                ]
                []

          else
            text ""
        , -- Hide the sidebar while shopping, even if we "jump out" by long-pressing an item or something
          case model.shopping of
            Nothing ->
                viewSidebar model.page model.darkMode

            Just _ ->
                text ""
        , div
            [ class
                ("content"
                    ++ (if loading then
                            " loading"

                        else
                            ""
                       )
                    ++ (case model.page of
                            ShoppingPage _ ->
                                " shopping"

                            _ ->
                                ""
                       )
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


viewErrorBanner : String -> Html Msg
viewErrorBanner err =
    div
        [ class "error-banner" ]
        [ span [] [ text err ]
        , button [ onPointerup DismissError ] [ text "✕" ]
        ]


viewSidebar : Page -> Bool -> Html Msg
viewSidebar page darkMode =
    Html.nav
        [ class "sidebar" ]
        [ div
            [ class
                (if page == ShoppingSelectionPage then
                    "sidebar-shop active"

                 else
                    "sidebar-shop"
                )
            , onPointerup Shop
            ]
            [ span [ class "material-symbols-outlined fill" ] [ text "shopping_cart" ] ]
        , Html.hr [ class "sidebar-divider" ] []
        , Html.a
            [ href "/"
            , class
                (let
                    active =
                        case page of
                            ListPage ->
                                True

                            ItemPage _ ->
                                True

                            ItemStorePage _ _ ->
                                True

                            _ ->
                                False
                 in
                 ifte active "sidebar-list active" "sidebar-list"
                )
            ]
            [ span [ class "material-symbols-outlined fill" ] [ text "list" ] ]
        , Html.hr [ class "sidebar-divider-large" ] []
        , Html.a
            [ href (pageUrl StoresPage)
            , class
                (case page of
                    StoresPage ->
                        "sidebar-stores active"

                    StorePage _ ->
                        "sidebar-stores active"

                    StoreItemPage _ _ ->
                        "sidebar-stores active"

                    _ ->
                        "sidebar-stores"
                )
            ]
            [ span [ class "material-symbols-outlined" ] [ text "store" ] ]
        , Html.hr [ class "sidebar-divider" ] []
        , button
            [ class "dark-mode-toggle"
            , onPointerup ToggleDarkMode
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

        ItemPage itemId ->
            viewFabWith
                model
                "Star Market"
                (CreateStore (Just itemId))
                (case model.fabValue of
                    Nothing ->
                        []

                    Just value ->
                        let
                            value1 =
                                String.toLower value
                        in
                        model.stores
                            |> Dict.values
                            |> List.filter
                                (\store ->
                                    String.startsWith value1 (String.toLower store.name)
                                        && (case Dict.get itemId model.itemStores of
                                                Nothing ->
                                                    True

                                                Just itemStores ->
                                                    case Dict.get store.id itemStores of
                                                        Nothing ->
                                                            True

                                                        Just itemStore ->
                                                            not itemStore.sold
                                           )
                                )
                            |> List.take 5
                            |> List.sortBy (.name >> String.toLower)
                            |> List.map (\store -> ( store.name, ItemInStore itemId store.id Nothing ))
                )

        ItemStorePage _ storeId ->
            viewSectionFab model storeId

        StorePage storeId ->
            if model.storePageTab then
                viewSectionFab model storeId

            else
                viewFabWith
                    model
                    "milk"
                    (AddItemNameToStore storeId)
                    -- FIXME we can be more efficient than AddItemToStore here
                    (fabItemSuggestions model (\item -> AddItemNameToStore storeId item.name))

        StoreItemPage storeId _ ->
            viewSectionFab model storeId

        -- TODO
        StoreSectionPage _ _ ->
            []

        -- TODO
        StoreSectionItemPage _ _ _ ->
            []

        StoresPage ->
            viewFabWith model "Star Market" (CreateStore Nothing) []

        ShoppingItemPage storeId _ ->
            viewSectionFab model storeId

        ShoppingPage _ ->
            viewListFab model

        ShoppingSelectionPage ->
            viewFabWith model "Star Market" (CreateStore Nothing) []


viewListFab : Model -> List (Html Msg)
viewListFab model =
    viewFabWith
        model
        "milk"
        AddItemNameToList
        (fabItemSuggestions model (\item -> AddItemIdToList item.id))


viewSectionFab : Model -> StoreId -> List (Html Msg)
viewSectionFab model storeId =
    viewFabWith model "Aisle 1" (CreateSection storeId) []


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
            div [ class "scrim", onPointerup FabClose ] []
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
                            , onPointerup msg
                            , preventDefaultOn "mousedown" (Decode.succeed ( Noop, True )) -- prevent blur
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
                    , onPointerup FabOpen
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

        ItemPage itemId ->
            case Dict.get itemId model.items of
                Just item ->
                    viewItemPage model item

                Nothing ->
                    []

        ItemStorePage itemId storeId ->
            case ( Dict.get storeId model.stores, Dict.get itemId model.items ) of
                ( Just store, Just item ) ->
                    viewStoreAndItemPageHelper model store item

                _ ->
                    []

        ShoppingItemPage storeId itemId ->
            case ( Dict.get storeId model.stores, Dict.get itemId model.items ) of
                ( Just store, Just item ) ->
                    viewStoreAndItemPageHelper model store item

                _ ->
                    []

        ShoppingPage storeId ->
            case Dict.get storeId model.stores of
                Just store ->
                    viewShoppingPage model store

                Nothing ->
                    []

        ShoppingSelectionPage ->
            viewShoppingSelectionPage model

        StorePage storeId ->
            case Dict.get storeId model.stores of
                Just store ->
                    viewStorePage model store

                Nothing ->
                    []

        StoreItemPage storeId itemId ->
            case ( Dict.get storeId model.stores, Dict.get itemId model.items ) of
                ( Just store, Just item ) ->
                    viewStoreAndItemPageHelper model store item

                _ ->
                    []

        StoreSectionPage _ sectionId ->
            case Dict.get sectionId model.sections of
                Just section ->
                    viewSectionPage model section

                Nothing ->
                    []

        StoreSectionItemPage storeId _ itemId ->
            case ( Dict.get storeId model.stores, Dict.get itemId model.items ) of
                ( Just store, Just item ) ->
                    viewStoreAndItemPageHelper model store item

                _ ->
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

        pointerEventHandlers : ItemId -> List (Html.Attribute Msg)
        pointerEventHandlers item =
            [ onPointercancel PointercancelItem
            , onPointerdown (PointerdownItem item)
            , onPointerup (PointerupItem item)
            ]

        viewOffListItem : Item -> Html Msg
        viewOffListItem item =
            li
                (class "long-pressable" :: pointerEventHandlers item.id)
                [ div
                    [ class "muted" ]
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
                    (class "long-pressable" :: pointerEventHandlers item.id)
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
    , let
        numItems =
            Dict.size model.items
      in
      if numItems > 0 && numItems <= 5 then
        div
            [ class "tip" ]
            [ text "You can long-press an item to rename it, delete it, and more." ]

      else
        text ""
    ]


viewSectionPage : Model -> Section -> List (Html Msg)
viewSectionPage model section =
    let
        viewItem : Item -> Html msg
        viewItem item =
            li
                []
                [ Html.a
                    [ href (pageUrl (StoreItemPage section.store item.id)) ]
                    [ span
                        []
                        [ text item.name
                        , smallMutedSymbol "chevron_right"
                        ]
                    ]
                ]
    in
    [ div
        [ class "page-header" ]
        [ button
            [ onPointerup ClickedBack ]
            [ smallMutedSymbol "arrow_back" ]
        , h1 [] [ editNameInput (Maybe.withDefault section.name model.editingName) ]
        , if model.confirmingDelete then
            button
                [ class "no"
                , onPointerup (DeleteSection section)
                ]
                [ span
                    [ class "material-symbols-outlined icon-sm" ]
                    [ text "delete" ]
                , text "Delete"
                ]

          else
            button
                [ onPointerup ConfirmDelete ]
                [ smallMutedSymbol "delete" ]
        ]
    , model.itemStores
        |> Dict.foldl
            (\itemId itemStores acc ->
                case Dict.get section.store itemStores of
                    Nothing ->
                        acc

                    Just itemStore ->
                        if itemStore.section == Just section.id then
                            case Dict.get itemId model.items of
                                Just item ->
                                    item :: acc

                                Nothing ->
                                    acc

                        else
                            acc
            )
            []
        |> List.sortBy (.name >> String.toLower)
        |> List.map viewItem
        |> ul []
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
                                , case itemStore.section of
                                    Nothing ->
                                        text ""

                                    Just section ->
                                        viewSectionLabel model section
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
                            [ span [] [ text store.name, chevron ] ]
                ]
    in
    [ div
        [ class "page-header" ]
        [ button
            [ onPointerup ClickedBack ]
            [ smallMutedSymbol "arrow_back" ]
        , h1 [] [ editNameInput (Maybe.withDefault item.name model.editingName) ]
        , if model.confirmingDelete then
            button
                [ class "no"
                , onPointerup (DeleteItem item.id)
                ]
                [ span
                    [ class "material-symbols-outlined icon-sm" ]
                    [ text "delete" ]
                , text "Delete"
                ]

          else
            button
                [ onPointerup ConfirmDelete ]
                [ smallMutedSymbol "delete" ]
        ]
    , model.stores
        |> Dict.values
        |> List.sortBy (.name >> String.toLower)
        |> List.map viewStore
        |> ul []
    ]


viewShoppingPage : Model -> Store -> List (Html Msg)
viewShoppingPage model store =
    let
        f :
            ItemId
            -> Item
            -> ( List ( Item, Bool ), Dict SectionId (List Item) )
            -> ( List ( Item, Bool ), Dict SectionId (List Item) )
        f itemId item (( acc1, acc2 ) as acc) =
            if item.onList then
                let
                    maybeItemStore : Maybe ItemStore
                    maybeItemStore =
                        model.itemStores |> Dict.get itemId |> Maybe.andThen (Dict.get store.id)
                in
                case maybeItemStore of
                    Just itemStore ->
                        if itemStore.sold then
                            case itemStore.section of
                                Nothing ->
                                    ( ( item, True ) :: acc1
                                    , acc2
                                    )

                                Just section ->
                                    ( acc1
                                    , dictUpsert section (Maybe.withDefault [] >> (::) item) acc2
                                    )

                        else
                            acc

                    Nothing ->
                        ( ( item, False ) :: acc1, acc2 )

            else
                acc

        ( sectionlessItems, sectionfulItems ) =
            Dict.foldl f ( [], Dict.empty ) model.items
    in
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
        , h1 [] [ text store.name ]
        ]
    , if List.isEmpty sectionlessItems then
        text ""

      else
        ul
            [ class "shop-items no-section" ]
            (sectionlessItems
                |> List.sortBy (\( item, _ ) -> String.toLower item.name)
                |> List.map (\( item, known ) -> viewShoppingPageItem item known)
            )
    ]
        ++ (sectionfulItems
                |> Dict.toList
                |> List.filterMap
                    (\( sectionId, items ) ->
                        model.sections
                            |> Dict.get sectionId
                            |> Maybe.map (\section -> ( section, items ))
                    )
                |> List.sortBy (Tuple.first >> .position)
                |> List.concatMap
                    (\( section, items ) ->
                        [ viewSeparator section.name
                        , ul
                            [ class "shop-items" ]
                            (items
                                |> List.sortBy (.name >> String.toLower)
                                |> List.map (\item -> viewShoppingPageItem item True)
                            )
                        ]
                    )
           )
        ++ [ button
                [ class "shopping-done"
                , onPointerup DoneShopping
                ]
                [ span
                    [ class "material-symbols-outlined icon-sm" ]
                    [ text "check_circle" ]
                , text "Done shopping!"
                ]
           ]


viewShoppingPageItem : Item -> Bool -> Html Msg
viewShoppingPageItem item known =
    li
        [ class "long-pressable"
        , onPointercancel PointercancelItem
        , onPointerdown (PointerdownItem item.id)
        , onPointerup (PointerupItem item.id)
        ]
        [ div
            []
            [ smallMutedSymbol (ifte known "check" "question_mark")
            , span [] [ text item.name ]
            ]
        ]


viewShoppingSelectionPage : Model -> List (Html Msg)
viewShoppingSelectionPage model =
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
    , if Dict.isEmpty model.stores then
        div
            [ class "tip" ]
            [ text "You can create a new store below, or on the stores tab." ]

      else
        text ""
    ]


viewStorePage : Model -> Store -> List (Html Msg)
viewStorePage model store =
    [ div
        [ class "page-header" ]
        [ button
            [ onPointerup ClickedBack ]
            [ smallMutedSymbol "arrow_back" ]
        , h1 [] [ editNameInput (Maybe.withDefault store.name model.editingName) ]
        , if model.confirmingDelete then
            button
                [ class "no"
                , onPointerup (DeleteStore store.id)
                ]
                [ span
                    [ class "material-symbols-outlined icon-sm" ]
                    [ text "delete" ]
                , text "Delete"
                ]

          else
            button
                [ onPointerup ConfirmDelete ]
                [ smallMutedSymbol "delete" ]
        ]
    , div
        [ class "tabs" ]
        [ button
            [ class (ifte model.storePageTab "active" "")
            , onPointerup (SetStorePageTab True)
            ]
            [ text "Sections" ]
        , button
            [ class (ifte model.storePageTab "" "active")
            , onPointerup (SetStorePageTab False)
            ]
            [ text "Items" ]
        ]
    , if model.storePageTab then
        case model.reorderingSections of
            _ :: _ :: _ ->
                div
                    []
                    [ let
                        viewSection : Int -> SectionId -> Html Msg
                        viewSection index sectionId =
                            li
                                [ class "reorder-item"
                                , onPointerup (ReorderSectionsMoveUp index)
                                ]
                                [ div []
                                    [ text
                                        (case Dict.get sectionId model.sections of
                                            Just section ->
                                                section.name

                                            Nothing ->
                                                ""
                                        )
                                    ]
                                , span [ class "material-symbols-outlined icon-sm muted padding-right-4px" ] [ text "arrow_upward" ]
                                ]
                      in
                      ul [] (List.indexedMap viewSection model.reorderingSections)
                    , button
                        [ class "fab-button reorder-button"
                        , onPointerup (ReorderSectionsDone store.id)
                        ]
                        [ smallMutedSymbol "edit_off" ]
                    ]

            _ ->
                let
                    sections : List Section
                    sections =
                        model.sections
                            |> Dict.values
                            |> List.filter (\section -> section.store == store.id)
                            |> List.sortBy .position
                in
                if List.isEmpty sections then
                    div [ class "tip" ] [ text "You can create a new section below." ]

                else
                    div
                        []
                        [ let
                            viewSection : Section -> Html Msg
                            viewSection section =
                                li
                                    []
                                    [ Html.a
                                        [ href (pageUrl (StoreSectionPage section.store section.id)) ]
                                        [ span
                                            []
                                            [ text section.name
                                            , smallMutedSymbol "chevron_right"
                                            ]
                                        ]
                                    ]
                          in
                          sections
                            |> List.map viewSection
                            |> ul []
                        , case sections of
                            -- Can't reorder one section :)
                            [ _ ] ->
                                text ""

                            _ ->
                                button
                                    [ class "fab-button reorder-button"
                                    , onPointerup (ReorderSectionsStart store.id)
                                    ]
                                    [ smallMutedSymbol "edit" ]
                        ]

      else
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
                                    , case itemStore.section of
                                        Nothing ->
                                            text ""

                                        Just section ->
                                            viewSectionLabel model section
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
                                [ span [] [ text item.name, chevron ]
                                ]
                    ]
        in
        model.items
            |> Dict.values
            |> List.sortBy (.name >> String.toLower)
            |> List.map viewItem
            |> ul []
    ]


viewStoreAndItemPageHelper : Model -> Store -> Item -> List (Html Msg)
viewStoreAndItemPageHelper model store item =
    let
        maybeItemStore : Maybe ItemStore
        maybeItemStore =
            Dict.get item.id model.itemStores |> Maybe.andThen (Dict.get store.id)

        sections : List Section
        sections =
            List.sortBy (.name >> String.toLower) (getStoreSections model store.id)

        viewSection : Section -> Html Msg
        viewSection section =
            let
                selected =
                    case maybeItemStore of
                        Nothing ->
                            False

                        Just itemStore ->
                            case itemStore.section of
                                Just section1 ->
                                    section1 == section.id

                                Nothing ->
                                    False
            in
            li
                [ class (ifte selected "active" "")
                , onPointerup (PointerupSection (Just (Just section.id)))
                ]
                [ div [] [ text section.name ] ]
    in
    [ div
        [ class "page-header" ]
        [ button
            [ onPointerup ClickedBack ]
            [ smallMutedSymbol "arrow_back" ]
        , h1 [] [ text (item.name ++ " at " ++ store.name) ]
        ]
    , ul
        []
        (List.map viewSection sections
            ++ [ let
                    selected =
                        case maybeItemStore of
                            Nothing ->
                                False

                            Just itemStore ->
                                itemStore.sold && isNothing itemStore.section
                 in
                 li
                    [ class (ifte selected "active" "")
                    , onPointerup (PointerupSection (Just Nothing))
                    ]
                    [ div
                        []
                        [ text (ifte (List.isEmpty sections) "Sold here" "Somewhere") ]
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
                    [ class (ifte selected "active" "")
                    , onPointerup (PointerupSection Nothing)
                    ]
                    [ div [] [ text "Not sold here" ] ]
               ]
        )
    , let
        showTip =
            case model.shopping of
                Nothing ->
                    List.isEmpty sections

                Just _ ->
                    True
      in
      if showTip then
        div [ class "tip" ] [ text "You can create a new section below." ]

      else
        text ""
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
        [ div
            [ class "page-header" ]
            [ smallMutedSymbol "store"
            , h1 [] [ text "Stores" ]
            ]
        , model.stores
            |> Dict.values
            |> List.sortBy (.name >> String.toLower)
            |> List.map viewStore
            |> ul []
        ]
    ]


viewSectionLabel : Model -> SectionId -> Html Msg
viewSectionLabel model sectionId =
    case Dict.get sectionId model.sections of
        Just section ->
            viewSectionLabel1 section

        Nothing ->
            text ""


viewSectionLabel1 : Section -> Html Msg
viewSectionLabel1 section =
    span [ class "section-label" ] [ text section.name ]


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
    | RequestCreateSection CreateSectionRequest
    | RequestCreateStore CreateStoreRequest
    | RequestDeleteItem DeleteItemRequest
    | RequestDeleteSection StoreId DeleteSectionRequest
    | RequestDeleteStore DeleteStoreRequest
    | RequestGetItems GetItemsRequest
    | RequestItemInStore ItemInStoreRequest
    | RequestItemNotInStore ItemNotInStoreRequest
    | RequestMoveItemOff MoveItemOffRequest
    | RequestMoveItemOn MoveItemOnRequest
    | RequestRenameItem RenameItemRequest
    | RequestRenameSection RenameSectionRequest
    | RequestRenameStore RenameStoreRequest
    | RequestReorderSections ReorderSectionsRequest


type alias CreateItemRequest =
    { name : String
    , onList : Bool
    , store : Maybe StoreId
    }


type alias CreateSectionRequest =
    { store : StoreId
    , name : String
    }


type alias CreateStoreRequest =
    { name : String
    , item : Maybe ItemId
    }


type alias DeleteItemRequest =
    { id : ItemId
    }


type alias DeleteSectionRequest =
    { id : SectionId
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
    , section : Maybe SectionId
    }


type alias ItemNotInStoreRequest =
    { item : ItemId
    , store : StoreId
    }


type alias MoveItemOffRequest =
    { item : ItemId
    }


type alias MoveItemOnRequest =
    { item : ItemId
    , store : Maybe StoreId
    }


type alias ReorderSectionsRequest =
    { store : StoreId
    , sections : List SectionId
    }


type alias RenameItemRequest =
    { id : ItemId
    , name : String
    }


type alias RenameSectionRequest =
    { id : SectionId
    , store : StoreId
    , name : String
    }


type alias RenameStoreRequest =
    { id : StoreId
    , name : String
    }


{-| FIXME don't have to tuck requests into responses anymore because we have request queue
-}
type Response
    = ResponseCreateItem CreateItemRequest (Result Http.Error CreateItemResponse)
    | ResponseCreateSection CreateSectionRequest (Result Http.Error CreateSectionResponse)
    | ResponseCreateStore CreateStoreRequest (Result Http.Error CreateStoreResponse)
    | ResponseDeleteItem DeleteItemRequest (Result Http.Error DeleteItemResponse)
    | ResponseDeleteSection StoreId DeleteSectionRequest (Result Http.Error DeleteSectionResponse)
    | ResponseDeleteStore DeleteStoreRequest (Result Http.Error DeleteStoreResponse)
    | ResponseGetItems (Result Http.Error GetItemsResponse)
    | ResponseItemInStore ItemInStoreRequest (Result Http.Error ItemInStoreResponse)
    | ResponseItemNotInStore ItemNotInStoreRequest (Result Http.Error ItemNotInStoreResponse)
    | ResponseMoveItemOff MoveItemOffRequest (Result Http.Error MoveItemOffResponse)
    | ResponseMoveItemOn MoveItemOnRequest (Result Http.Error MoveItemOnResponse)
    | ResponseRenameItem RenameItemRequest (Result Http.Error RenameItemResponse)
    | ResponseRenameSection RenameSectionRequest (Result Http.Error RenameSectionResponse)
    | ResponseRenameStore RenameStoreRequest (Result Http.Error RenameStoreResponse)
    | ResponseReorderSections ReorderSectionsRequest (Result Http.Error ReorderSectionsResponse)


type alias CreateItemResponse =
    { dataVersion : Int
    , id : ItemId
    }


type alias CreateSectionResponse =
    { dataVersion : Int
    , id : SectionId
    , position : Int
    }


type alias CreateStoreResponse =
    { dataVersion : Int
    , id : StoreId
    }


type alias DeleteItemResponse =
    { dataVersion : Int
    }


type alias DeleteSectionResponse =
    { dataVersion : Int
    }


type alias DeleteStoreResponse =
    { dataVersion : Int
    }


type alias GetItemsResponse =
    { dataVersion : Int
    , items : List Item
    , itemStores : List ItemStore
    , sections : List Section
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


type alias RenameSectionResponse =
    { dataVersion : Int
    }


type alias RenameStoreResponse =
    { dataVersion : Int
    }


type alias ReorderSectionsResponse =
    { dataVersion : Int
    }


sendRequest : Request -> Cmd Msg
sendRequest request =
    case request of
        RequestCreateItem request1 ->
            sendCreateItemRequest request1

        RequestCreateSection request1 ->
            sendCreateSectionRequest request1

        RequestCreateStore request1 ->
            sendCreateStoreRequest request1

        RequestDeleteItem request1 ->
            sendDeleteItemRequest request1

        RequestDeleteSection storeId request1 ->
            sendDeleteSectionRequest storeId request1

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

        RequestRenameSection request1 ->
            sendRenameSectionRequest request1

        RequestRenameStore request1 ->
            sendRenameStoreRequest request1

        RequestReorderSections request1 ->
            sendReorderSectionsRequest request1


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


sendCreateSectionRequest : CreateSectionRequest -> Cmd Msg
sendCreateSectionRequest request =
    let
        body =
            Encode.object
                [ ( "store", Encode.int request.store )
                , ( "name", Encode.string request.name )
                ]
    in
    Http.post
        { url = "/api/create-section"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseCreateSection request)
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
                , ( "item"
                  , case request.item of
                        Nothing ->
                            Encode.null

                        Just item ->
                            Encode.int item
                  )
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


sendDeleteSectionRequest : StoreId -> DeleteSectionRequest -> Cmd Msg
sendDeleteSectionRequest storeId request =
    let
        body =
            Encode.object
                [ ( "id", Encode.int request.id )
                ]
    in
    Http.post
        { url = "/api/delete-section"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseDeleteSection storeId request)
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
                    (\dataVersion items itemStores sections stores ->
                        { dataVersion = dataVersion
                        , items = items
                        , itemStores = itemStores
                        , sections = sections
                        , stores = stores
                        }
                    )
                    (Decode.field "data_version" Decode.int)
                    (Decode.field "items" (Decode.list itemDecoder))
                    (Decode.field "item_stores" (Decode.list itemStoreDecoder))
                    (Decode.field "sections" (Decode.list sectionDecoder))
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
                , ( "section"
                  , case request.section of
                        Just section ->
                            Encode.int section

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


sendRenameSectionRequest : RenameSectionRequest -> Cmd Msg
sendRenameSectionRequest request =
    let
        body =
            Encode.object
                [ ( "id", Encode.int request.id )
                , ( "store", Encode.int request.store )
                , ( "name", Encode.string request.name )
                ]
    in
    Http.post
        { url = "/api/rename-section"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseRenameSection request)
                (Decode.map
                    (\dataVersion -> { dataVersion = dataVersion })
                    (Decode.field "data_version" Decode.int)
                )
        }


sendReorderSectionsRequest : ReorderSectionsRequest -> Cmd Msg
sendReorderSectionsRequest request =
    let
        body : Value
        body =
            Encode.object
                [ ( "sections", Encode.list Encode.int request.sections )
                , ( "store", Encode.int request.store )
                ]
    in
    Http.post
        { url = "/api/reorder-sections"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseReorderSections request)
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

        RequestCreateSection _ ->
            model

        RequestCreateStore _ ->
            model

        RequestDeleteItem request1 ->
            assumeDeleteItemRequestWillSucceed request1 model

        RequestDeleteSection storeId request1 ->
            assumeDeleteSectionRequestWillSucceed storeId request1 model

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

        RequestRenameSection request1 ->
            assumeRenameSectionRequestWillSucceed request1 model

        RequestRenameStore request1 ->
            assumeRenameStoreRequestWillSucceed request1 model

        RequestReorderSections request1 ->
            assumeReorderSectionsRequestWillSucceed request1 model


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


assumeDeleteSectionRequestWillSucceed : StoreId -> DeleteSectionRequest -> Model -> Model
assumeDeleteSectionRequestWillSucceed storeId request model =
    { model
        | itemStores =
            Dict.map
                (\_ ->
                    dictUpdateIfExists
                        storeId
                        (\itemStore ->
                            if itemStore.section == Just request.id then
                                { itemStore | section = Nothing }

                            else
                                itemStore
                        )
                )
                model.itemStores
        , sections = Dict.remove request.id model.sections
    }


assumeDeleteStoreRequestWillSucceed : DeleteStoreRequest -> Model -> Model
assumeDeleteStoreRequestWillSucceed request model =
    { model
        | itemStores = Dict.map (\_ itemStores -> Dict.remove request.id itemStores) model.itemStores
        , sections = Dict.filter (\_ section -> section.store /= request.id) model.sections
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
                        , section = request.section
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
                        , section = Nothing
                        }
                )
                model.itemStores
    }


assumeMoveItemOffRequestWillSucceed : MoveItemOffRequest -> Model -> Model
assumeMoveItemOffRequestWillSucceed request model =
    { model
        | items =
            dictUpdateIfExists
                request.item
                (\item -> { item | onList = False })
                model.items
        , listPageJustMovedOffItems =
            case model.page of
                ListPage ->
                    Set.insert request.item model.listPageJustMovedOffItems

                _ ->
                    model.listPageJustMovedOffItems
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
                                            , section = Nothing
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


assumeRenameSectionRequestWillSucceed : RenameSectionRequest -> Model -> Model
assumeRenameSectionRequestWillSucceed request model =
    { model
        | sections =
            dictUpdateIfExists
                request.id
                (\section -> { section | name = request.name })
                model.sections
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


assumeReorderSectionsRequestWillSucceed : ReorderSectionsRequest -> Model -> Model
assumeReorderSectionsRequestWillSucceed =
    -- Set sections within a model to be in the order of the given list (e.g. the first section is position 0, the
    -- second is position 1...). This is used both when sending a reorder request (optimistically assuming success) and
    -- also when receiving an error response to a reorder request (to roll back the assumption).
    let
        go position sectionIds sections =
            case sectionIds of
                sectionId :: sectionIds1 ->
                    go
                        (position + 1)
                        sectionIds1
                        (dictUpdateIfExists
                            sectionId
                            (\section -> { section | position = position })
                            sections
                        )

                [] ->
                    sections
    in
    \request model -> { model | sections = go 0 request.sections model.sections }


{-| Does this store have any sections?
-}



{-
   doesStoreHaveSections : Model -> StoreId -> Bool
   doesStoreHaveSections model storeId =
       let
           loop : List Section -> Bool
           loop sections =
               case sections of
                   section :: sections1 ->
                       if section.store == storeId then
                           True

                       else
                           loop sections1

                   [] ->
                       False
       in
       loop (Dict.values model.sections)
-}


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


getStoreByName : Model -> String -> Maybe Store
getStoreByName model name =
    listFind (\store -> store.name == name) (Dict.values model.stores)


{-| Get a store's sections (sorted arbitrarily)
-}
getStoreSections : Model -> StoreId -> List Section
getStoreSections model storeId =
    model.sections
        |> Dict.values
        |> List.filter (\section -> section.store == storeId)


{-| Is a particular item known to be sold at a particular store? Returns false if either we don't know, or no.
-}
isItemKnownToBeSoldAtStore : Model -> StoreId -> ItemId -> Bool
isItemKnownToBeSoldAtStore model storeId itemId =
    case Dict.get itemId model.itemStores |> Maybe.andThen (Dict.get storeId) of
        Nothing ->
            False

        Just itemStore ->
            itemStore.sold


pushPage : Page -> Eff Model Msg -> Eff Model Msg
pushPage page value =
    value
        |> andThenCommand (\model -> Navigation.pushUrl model.key (pageUrl page))


replacePage : Page -> Eff Model Msg -> Eff Model Msg
replacePage page value =
    value
        |> andThenCommand (\model -> Navigation.replaceUrl model.key (pageUrl page))



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
    , section : Maybe SectionId
    }


itemStoreDecoder : Decoder ItemStore
itemStoreDecoder =
    Decode.map4
        ItemStore
        (Decode.field "item" Decode.int)
        (Decode.field "store" Decode.int)
        (Decode.field "sold" Decode.bool)
        (Decode.field "section" (Decode.nullable Decode.int))



------------------------------------------------------------------------------------------------------------------------
-- Section type


type alias SectionId =
    Int


type alias Section =
    { id : SectionId
    , store : StoreId
    , position : Int
    , name : String
    }


sectionDecoder : Decoder Section
sectionDecoder =
    Decode.map4
        Section
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


andThenCommand : (model -> Cmd msg) -> Eff model msg -> Eff model msg
andThenCommand f ( model, cmd ) =
    ( model, Cmd.batch [ cmd, f model ] )


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


ifte : Bool -> a -> a -> a
ifte b x y =
    if b then
        x

    else
        y


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


onPointercancel : msg -> Html.Attribute msg
onPointercancel msg =
    Html.Events.on "pointercancel" (Decode.succeed msg)


onPointerdown : msg -> Html.Attribute msg
onPointerdown msg =
    preventDefaultOn "pointerdown" (Decode.succeed ( msg, True ))


onPointerup : msg -> Html.Attribute msg
onPointerup msg =
    Html.Events.on "pointerup" (Decode.succeed msg)


{-| Get the intersection of a list of sets.
-}
setIntersects : List (Set comparable) -> Set comparable
setIntersects xs =
    case xs of
        x :: xs1 ->
            List.foldl Set.intersect x xs1

        [] ->
            Set.empty
