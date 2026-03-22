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
import Html.Attributes exposing (attribute, autocomplete, class, classList, href, id, placeholder, rows, type_, value)
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
    , -- On the list page, items mid-animation (moving between on/off). Direction is inferred from item.onList.
      listPageAnimatingItems : Set ItemId
    , longpressed : Bool
    , page : Page
    , reorderingSections : List SectionId
    , -- Outstanding HTTP requests. The front of the queue is in-flight.
      requests : Queue Request
    , sections : Dict SectionId Section
    , shopping : Maybe StoreId
    , storePageTab : StorePageTab
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
    | AddItemIdToListAndStore ItemId StoreId
    | AddItemNameToList String
    | AddItemNameToListAndStore String StoreId
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
    | ListPageItemDoneAnimating ItemId
    | LongpressItem Int ItemId
    | PointercancelItem
    | PointerdownItem ItemId
    | PointerupItem ItemId
    | PointerupSection SectionSpecification
    | Noop
    | ReorderSectionsDone StoreId
    | ReorderSectionsMoveUp Int
    | ReorderSectionsStart StoreId
    | Response Response
    | SetStorePageTab StorePageTab
    | Shop
    | ToggleDarkMode
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest


type StorePageTab
    = StorePageSectionsTab
    | StorePageItemsTab


type SectionSpecification
    = NotSoldHere
    | SoldHereSomewhere
    | SoldHereInSection SectionId


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
        [ Url.Parser.map
            ListPage
            Url.Parser.top
        , Url.Parser.map
            ItemPage
            (Url.Parser.s "item"
                </> Url.Parser.int
            )
        , Url.Parser.map
            ItemStorePage
            (Url.Parser.s "item"
                </> Url.Parser.int
                </> Url.Parser.s "store"
                </> Url.Parser.int
            )
        , Url.Parser.map
            ShoppingItemPage
            (Url.Parser.s "shop"
                </> Url.Parser.int
                </> Url.Parser.s "item"
                </> Url.Parser.int
            )
        , Url.Parser.map
            ShoppingPage
            (Url.Parser.s "shop"
                </> Url.Parser.int
            )
        , Url.Parser.map
            ShoppingSelectionPage
            (Url.Parser.s "shop")
        , Url.Parser.map
            StoreItemPage
            (Url.Parser.s "store"
                </> Url.Parser.int
                </> Url.Parser.s "item"
                </> Url.Parser.int
            )
        , Url.Parser.map
            StorePage
            (Url.Parser.s "store"
                </> Url.Parser.int
            )
        , Url.Parser.map
            StoreSectionItemPage
            (Url.Parser.s "store"
                </> Url.Parser.int
                </> Url.Parser.s "section"
                </> Url.Parser.int
                </> Url.Parser.s "item"
                </> Url.Parser.int
            )
        , Url.Parser.map
            StoreSectionPage
            (Url.Parser.s "store"
                </> Url.Parser.int
                </> Url.Parser.s "section"
                </> Url.Parser.int
            )
        , Url.Parser.map
            StoresPage
            (Url.Parser.s "stores")
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


init : Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
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
            , listPageAnimatingItems = Set.empty
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
            , storePageTab = StorePageSectionsTab
            , stores = Dict.empty
            , tapCount = 0
            }
    in
    case Url.Parser.parse pageParser url of
        Just page ->
            model page
                |> enqueueRequest (RequestGetItems { dataVersion = dataVersion })

        Nothing ->
            model ListPage
                |> updateSequence
                    [ enqueueRequest (RequestGetItems { dataVersion = dataVersion })
                    , updateCommand_ (Navigation.replaceUrl key (pageUrl ListPage))
                    ]


update : Msg -> Update Msg Model
update msg =
    case msg of
        AddItemIdToList itemId ->
            handleAddItemIdToList itemId

        AddItemIdToListAndStore itemId storeId ->
            handleAddItemIdToListAndStore itemId storeId

        AddItemNameToList name ->
            handleAddItemNameToList name

        AddItemNameToListAndStore name storeId ->
            handleAddItemNameToListAndStore name storeId

        Blur id ->
            handleBlur id

        ClearLongpressed ->
            handleClearLongpressed

        ClickedBack ->
            handleClickedBack

        ConfirmDelete ->
            handleConfirmDelete

        CreateItem name ->
            handleCreateItem name

        CreateSection storeId name ->
            handleCreateSection storeId name

        CreateStore item name ->
            handleCreateStore item name

        DocumentBecameVisible ->
            handleDocumentBecameVisible

        DoneShopping ->
            handleDoneShopping

        DeleteItem id ->
            handleDeleteItem id

        DeleteSection section ->
            handleDeleteSection section

        DeleteStore id ->
            handleDeleteStore id

        DismissError ->
            handleDismissError

        EditName ->
            handleEditName

        EditNameChanged name ->
            handleEditNameChanged name

        EditNameSubmit ->
            handleEditNameSubmit

        FabClose ->
            handleFabClose

        FabOpen ->
            handleFabOpen

        FabValueChanged value ->
            handleFabValueChanged value

        ListPageItemDoneAnimating itemId ->
            handleListPageItemDoneAnimating itemId

        LongpressItem tapCount itemId ->
            handleLongpressItem tapCount itemId

        Noop ->
            updateNone

        PointercancelItem ->
            handlePointercancelItem

        PointerdownItem itemId ->
            handlePointerdownItem itemId

        PointerupItem itemId ->
            handlePointerupItem itemId

        PointerupSection sectionSpecification ->
            handlePointerupSection sectionSpecification

        ReorderSectionsDone storeId ->
            handleReorderSectionsDone storeId

        ReorderSectionsMoveUp index ->
            handleReorderSectionsMoveUp index

        ReorderSectionsStart storeId ->
            handleReorderSectionsStart storeId

        Response response ->
            handleResponse response

        SetStorePageTab tab ->
            handleSetStorePageTab tab

        Shop ->
            handleShop

        ToggleDarkMode ->
            handleToggleDarkMode

        UrlChanged url ->
            handleUrlChanged url

        UrlRequested urlRequest ->
            handleUrlRequested urlRequest


handleAddItemIdToList : ItemId -> Update Msg Model
handleAddItemIdToList itemId =
    updateUsing
        assumeInFlightRequestsWillSucceed
        (\effectiveModel ->
            updateSequence
                [ updatePure (\model -> { model | fabValue = Nothing })
                , case Dict.get itemId effectiveModel.items of
                    Just item ->
                        if item.onList then
                            updateNone

                        else
                            enqueueRequest (RequestMoveItemOn { item = item.id })

                    Nothing ->
                        updateNone
                ]
        )


handleAddItemIdToListAndStore : ItemId -> StoreId -> Update Msg Model
handleAddItemIdToListAndStore itemId storeId =
    updateSequence
        [ updatePure (\model -> { model | fabValue = Nothing })
        , updateUsing
            assumeInFlightRequestsWillSucceed
            (\effectiveModel ->
                case Dict.get itemId effectiveModel.items of
                    Nothing ->
                        updateNone

                    Just item ->
                        addItemToListAndStore effectiveModel item storeId
            )
        ]


{-| Add an item to the list by name. It might already exist, in which case we'll just move it onto the list. Otherwise,
we'll create it.
-}
handleAddItemNameToList : String -> Update Msg Model
handleAddItemNameToList name =
    let
        name1 =
            String.trim name
    in
    updateSequence
        [ updatePure (\model -> { model | fabValue = Nothing })
        , if String.isEmpty name1 then
            updateNone

          else
            updateUsing
                assumeInFlightRequestsWillSucceed
                (\effectiveModel ->
                    case getItemByName effectiveModel name1 of
                        Nothing ->
                            enqueueRequest
                                (RequestCreateItem
                                    { name = name1
                                    , onList = True
                                    , store = Nothing
                                    }
                                )

                        Just item ->
                            if item.onList then
                                updateNone

                            else
                                enqueueRequest (RequestMoveItemOn { item = item.id })
                )
        ]


handleAddItemNameToListAndStore : String -> StoreId -> Update Msg Model
handleAddItemNameToListAndStore name storeId =
    let
        name1 =
            String.trim name
    in
    updateSequence
        [ updatePure (\model -> { model | fabValue = Nothing })
        , if String.isEmpty name1 then
            updateNone

          else
            updateUsing
                assumeInFlightRequestsWillSucceed
                (\effectiveModel ->
                    case getItemByName effectiveModel name1 of
                        Nothing ->
                            enqueueRequest
                                (RequestCreateItem
                                    { name = name1
                                    , onList = True
                                    , store = Just storeId
                                    }
                                )

                        Just item ->
                            addItemToListAndStore effectiveModel item storeId
                )
        ]


addItemToListAndStore : Model -> Item -> StoreId -> Update Msg Model
addItemToListAndStore model item storeId =
    updateSequence
        [ if item.onList then
            updateNone

          else
            enqueueRequest (RequestMoveItemOn { item = item.id })
        , if isItemKnownToBeSoldAtStore model storeId item.id then
            updateNone

          else
            enqueueRequest
                (RequestItemInStore
                    { item = item.id
                    , store = storeId
                    , section = Nothing
                    }
                )
        ]


handleBlur : String -> Update Msg model
handleBlur id =
    updateCommand_ (Task.attempt (\_ -> Noop) (Browser.Dom.blur id))


handleClearLongpressed : Update msg Model
handleClearLongpressed =
    updatePure (\model -> { model | longpressed = False })


handleClickedBack : Update Msg Model
handleClickedBack model =
    model
        |> (case model.page of
                ItemPage _ ->
                    replacePage
                        (case model.shopping of
                            Nothing ->
                                ListPage

                            Just storeId ->
                                ShoppingPage storeId
                        )

                ItemStorePage item _ ->
                    replacePage (ItemPage item)

                ShoppingItemPage store _ ->
                    replacePage (ShoppingPage store)

                StoreItemPage store _ ->
                    replacePage (StorePage store)

                StoreSectionPage store _ ->
                    replacePage (StorePage store)

                StoreSectionItemPage store section _ ->
                    replacePage (StoreSectionPage store section)

                StorePage _ ->
                    replacePage StoresPage

                _ ->
                    updateNone
           )


handleConfirmDelete : Update msg Model
handleConfirmDelete =
    updatePure (\model -> { model | confirmingDelete = True })


handleCreateItem : String -> Update Msg Model
handleCreateItem name =
    let
        name1 =
            String.trim name
    in
    updateSequence
        [ updatePure (\model -> { model | fabValue = Nothing })
        , updateUsing
            assumeInFlightRequestsWillSucceed
            (\effectiveModel ->
                if String.isEmpty name1 || isJust (getItemByName effectiveModel name1) then
                    updateNone

                else
                    enqueueRequest
                        (RequestCreateItem
                            { name = name1
                            , onList = False
                            , store = Nothing
                            }
                        )
            )
        ]


handleCreateSection : StoreId -> String -> Update Msg Model
handleCreateSection storeId name =
    let
        name1 : String
        name1 =
            String.trim name
    in
    updateSequence
        [ updatePure (\model -> { model | fabValue = Nothing })
        , if String.isEmpty name1 then
            updateNone

          else
            enqueueRequest
                (RequestCreateSection
                    { store = storeId
                    , name = name1
                    }
                )
        ]


handleCreateStore : Maybe ItemId -> String -> Update Msg Model
handleCreateStore item name =
    let
        name1 : String
        name1 =
            String.trim name
    in
    updateSequence
        [ updatePure (\model -> { model | fabValue = Nothing })
        , if String.isEmpty name1 then
            updateNone

          else
            updateUsing
                assumeInFlightRequestsWillSucceed
                (\effectiveModel ->
                    case getStoreByName effectiveModel name1 of
                        Nothing ->
                            enqueueRequest
                                (RequestCreateStore
                                    { name = name1
                                    , item = item
                                    }
                                )

                        Just _ ->
                            updateNone
                )
        ]


handleDocumentBecameVisible : Update Msg Model
handleDocumentBecameVisible =
    updateUsing
        (\model ->
            ( queueContains
                (\request ->
                    case request of
                        RequestGetItems _ ->
                            True

                        _ ->
                            False
                )
                model.requests
            , model.dataVersion
            )
        )
        (\( getItemsRequestAlreadyEnqueued, dataVersion ) ->
            if getItemsRequestAlreadyEnqueued then
                updateNone

            else
                enqueueRequest (RequestGetItems { dataVersion = dataVersion })
        )


handleDoneShopping : Update Msg Model
handleDoneShopping =
    updateSequence
        [ updatePure (\model -> { model | shopping = Nothing })
        , replacePage ListPage
        ]


handleDeleteItem : ItemId -> Update Msg Model
handleDeleteItem itemId =
    updateSequence
        [ updatePure (\model -> { model | confirmingDelete = False })
        , updateUsing
            assumeInFlightRequestsWillSucceed
            (\effectiveModel ->
                case Dict.get itemId effectiveModel.items of
                    Just _ ->
                        enqueueRequest (RequestDeleteItem { id = itemId })

                    Nothing ->
                        updateNone
            )
        ]


handleDeleteSection : Section -> Update Msg Model
handleDeleteSection section =
    updateSequence
        [ updatePure (\model -> { model | confirmingDelete = False })
        , updateUsing
            assumeInFlightRequestsWillSucceed
            (\effectiveModel ->
                case Dict.get section.id effectiveModel.sections of
                    Just _ ->
                        enqueueRequest (RequestDeleteSection section.store { id = section.id })

                    Nothing ->
                        updateNone
            )
        , replacePage (StorePage section.store)
        ]


handleDeleteStore : StoreId -> Update Msg Model
handleDeleteStore id =
    updateSequence
        [ updatePure (\model -> { model | confirmingDelete = False })
        , updateUsing
            assumeInFlightRequestsWillSucceed
            (\effectiveModel ->
                case Dict.get id effectiveModel.stores of
                    Just _ ->
                        enqueueRequest (RequestDeleteStore { id = id })

                    Nothing ->
                        updateNone
            )
        , replacePage StoresPage
        ]


handleDismissError : Update msg Model
handleDismissError =
    updatePure (\model -> { model | error = Nothing })


handleEditName : Update msg Model
handleEditName =
    updatePure
        (\model ->
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
                            { model
                                | confirmingDelete = False
                                , editingName = Just name
                            }

                        Nothing ->
                            { model | confirmingDelete = False }

                Just _ ->
                    model
        )


handleEditNameChanged : String -> Update msg Model
handleEditNameChanged name =
    updatePure (\model -> { model | editingName = Just name })


handleEditNameSubmit : Update Msg Model
handleEditNameSubmit =
    let
        editItemName : ItemId -> String -> Update Msg Model
        editItemName itemId newName =
            let
                newName1 : String
                newName1 =
                    String.trim newName
            in
            if String.isEmpty newName1 then
                updateNone

            else
                updateUsing
                    assumeInFlightRequestsWillSucceed
                    (\effectiveModel ->
                        case Dict.get itemId effectiveModel.items of
                            Just item ->
                                if newName1 /= item.name then
                                    enqueueRequest
                                        (RequestRenameItem
                                            { id = itemId
                                            , name = newName1
                                            }
                                        )

                                else
                                    updateNone

                            Nothing ->
                                updateNone
                    )

        editSectionName : SectionId -> String -> Update Msg Model
        editSectionName sectionId newName =
            let
                newName1 : String
                newName1 =
                    String.trim newName
            in
            if String.isEmpty newName1 then
                updateNone

            else
                updateUsing
                    assumeInFlightRequestsWillSucceed
                    (\effectiveModel ->
                        case Dict.get sectionId effectiveModel.sections of
                            Just section ->
                                if newName1 /= section.name then
                                    enqueueRequest
                                        (RequestRenameSection
                                            { id = sectionId
                                            , store = section.store
                                            , name = newName1
                                            }
                                        )

                                else
                                    updateNone

                            Nothing ->
                                updateNone
                    )

        editStoreName : StoreId -> String -> Update Msg Model
        editStoreName storeId newName =
            let
                newName1 : String
                newName1 =
                    String.trim newName
            in
            if String.isEmpty newName1 then
                updateNone

            else
                updateUsing
                    assumeInFlightRequestsWillSucceed
                    (\effectiveModel ->
                        case Dict.get storeId effectiveModel.stores of
                            Just store ->
                                if newName1 /= store.name then
                                    enqueueRequest
                                        (RequestRenameStore
                                            { id = storeId
                                            , name = newName1
                                            }
                                        )

                                else
                                    updateNone

                            Nothing ->
                                updateNone
                    )
    in
    updateSequence
        [ updateUsing
            (\model -> ( model.page, model.editingName ))
            (\info ->
                case info of
                    ( ItemPage itemId, Just newName ) ->
                        editItemName itemId newName

                    ( StoreSectionPage _ sectionId, Just newName ) ->
                        editSectionName sectionId newName

                    ( StorePage storeId, Just newName ) ->
                        editStoreName storeId newName

                    _ ->
                        updateNone
            )
        , updatePure (\model -> { model | editingName = Nothing })
        ]


handleFabClose : Update msg Model
handleFabClose =
    updatePure (\model -> { model | fabValue = Nothing })


handleFabOpen : Update Msg Model
handleFabOpen =
    updateSequence
        [ updatePure
            (\model ->
                { model
                    | confirmingDelete = False
                    , fabValue = Just ""
                }
            )
        , updateCommand_ (selectAll "fab-input")
        ]


handleFabValueChanged : String -> Update msg Model
handleFabValueChanged value =
    updatePure (\model -> { model | fabValue = Just value })


handleLongpressItem : Int -> ItemId -> Update Msg Model
handleLongpressItem c itemId =
    updateUsing
        (\model -> ( model.tapCount, model.page ))
        (\( tapCount, page ) ->
            if c == tapCount then
                case page of
                    ListPage ->
                        updateSequence
                            [ updatePure (\model -> { model | longpressed = True })
                            , pushPage (ItemPage itemId)
                            ]

                    ShoppingPage storeId ->
                        updateSequence
                            [ updatePure (\model -> { model | longpressed = True })
                            , pushPage (ShoppingItemPage storeId itemId)
                            ]

                    _ ->
                        updateNone

            else
                updateNone
        )


handlePointercancelItem : Update msg Model
handlePointercancelItem =
    updatePure (\model -> { model | tapCount = model.tapCount + 1 })


handlePointerdownItem : ItemId -> Update Msg Model
handlePointerdownItem itemId =
    updateCommand
        (\model ->
            Process.sleep 500 |> Task.perform (\_ -> LongpressItem model.tapCount itemId)
        )


handleListPageItemDoneAnimating : ItemId -> Update Msg Model
handleListPageItemDoneAnimating itemId =
    updatePure (\model -> { model | listPageAnimatingItems = Set.remove itemId model.listPageAnimatingItems })


handlePointerupItem : ItemId -> Update Msg Model
handlePointerupItem itemId =
    updateSequence
        [ updatePure
            (\model ->
                { model
                    | longpressed = False
                    , tapCount = model.tapCount + 1
                }
            )
        , updateUsing
            (\model -> ( model.page, model.listPageAnimatingItems, assumeInFlightRequestsWillSucceed model ))
            (\( page, animatingItems, effectiveModel ) ->
                case page of
                    ListPage ->
                        case Dict.get itemId effectiveModel.items of
                            Just item ->
                                if Set.member itemId animatingItems then
                                    updateNone

                                else
                                    updateSequence
                                        [ updatePure
                                            (\model ->
                                                { model
                                                    | listPageAnimatingItems =
                                                        Set.insert itemId model.listPageAnimatingItems
                                                }
                                            )
                                        , -- this sleep time agrees with css list-item-anim-duration
                                          updateCommand_
                                            (Process.sleep 200 |> Task.perform (\_ -> ListPageItemDoneAnimating itemId))
                                        , enqueueRequest
                                            (if item.onList then
                                                RequestMoveItemOff { item = itemId }

                                             else
                                                RequestMoveItemOn { item = itemId }
                                            )
                                        ]

                            Nothing ->
                                updateNone

                    ShoppingPage storeId ->
                        case Dict.get itemId effectiveModel.itemStores of
                            Just itemStores ->
                                case Dict.get storeId itemStores of
                                    Just itemStore ->
                                        if itemStore.sold then
                                            enqueueRequest (RequestMoveItemOff { item = itemId })

                                        else
                                            updateNone

                                    Nothing ->
                                        pushPage (ShoppingItemPage storeId itemId)

                            Nothing ->
                                pushPage (ShoppingItemPage storeId itemId)

                    _ ->
                        updateNone
            )
        ]


handlePointerupSection : SectionSpecification -> Update Msg Model
handlePointerupSection =
    let
        recordItemInStore : ItemInStoreRequest -> Update Msg Model
        recordItemInStore request =
            updateSequence
                [ -- close fab because this could come from item page fab
                  updatePure (\model -> { model | fabValue = Nothing })
                , updateUsing
                    assumeInFlightRequestsWillSucceed
                    (\effectiveModel ->
                        let
                            shouldSend =
                                case Dict.get request.item effectiveModel.itemStores of
                                    Nothing ->
                                        True

                                    Just itemStores ->
                                        case Dict.get request.store itemStores of
                                            Nothing ->
                                                True

                                            Just itemStore ->
                                                not itemStore.sold || (itemStore.section /= request.section)
                        in
                        if shouldSend then
                            enqueueRequest (RequestItemInStore request)

                        else
                            updateNone
                    )
                ]

        recordItemNotInStore : ItemNotInStoreRequest -> Update Msg Model
        recordItemNotInStore request =
            updateUsing
                assumeInFlightRequestsWillSucceed
                (\effectiveModel ->
                    let
                        itemNotSoldInStore =
                            case Dict.get request.item effectiveModel.itemStores of
                                Nothing ->
                                    False

                                Just itemStores ->
                                    case Dict.get request.store itemStores of
                                        Nothing ->
                                            False

                                        Just itemStore ->
                                            not itemStore.sold
                    in
                    if itemNotSoldInStore then
                        updateNone

                    else
                        enqueueRequest (RequestItemNotInStore request)
                )
    in
    \sectionSpecification ->
        let
            theSectionUpdate item store =
                case sectionSpecification of
                    SoldHereInSection section ->
                        recordItemInStore
                            { item = item
                            , store = store
                            , section = Just section
                            }

                    SoldHereSomewhere ->
                        recordItemInStore
                            { item = item
                            , store = store
                            , section = Nothing
                            }

                    NotSoldHere ->
                        recordItemNotInStore
                            { item = item
                            , store = store
                            }
        in
        updateUsing
            .page
            (\page ->
                case page of
                    ItemStorePage item store ->
                        theSectionUpdate item store

                    ShoppingItemPage store item ->
                        updateSequence
                            [ theSectionUpdate item store
                            , replacePage (ShoppingPage store)
                            ]

                    StoreItemPage store item ->
                        theSectionUpdate item store

                    StoreSectionItemPage store _ item ->
                        theSectionUpdate item store

                    _ ->
                        updateNone
            )


handleReorderSectionsDone : StoreId -> Update Msg Model
handleReorderSectionsDone storeId =
    updateUsing
        (\model -> ( model.reorderingSections, assumeInFlightRequestsWillSucceed model ))
        (\( reorderingSections, effectiveModel ) ->
            case reorderingSections of
                _ :: _ :: _ ->
                    let
                        currentSections =
                            getStoreSections effectiveModel storeId
                                |> List.sortBy .position
                                |> List.map .id
                    in
                    updateSequence
                        [ updatePure (\model -> { model | reorderingSections = [] })
                        , if reorderingSections /= currentSections then
                            enqueueRequest
                                (RequestReorderSections
                                    { store = storeId
                                    , sections = reorderingSections
                                    }
                                )

                          else
                            updateNone
                        ]

                _ ->
                    updateNone
        )


handleReorderSectionsMoveUp : Int -> Update msg Model
handleReorderSectionsMoveUp index =
    updatePure
        (\model ->
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
        )


handleReorderSectionsStart : StoreId -> Update msg Model
handleReorderSectionsStart storeId =
    updatePure
        (\model ->
            let
                effectiveModel =
                    assumeInFlightRequestsWillSucceed model
            in
            { model
                | reorderingSections =
                    getStoreSections effectiveModel storeId
                        |> List.sortBy .position
                        |> List.map .id
            }
        )


handleResponse : Response -> Update Msg Model
handleResponse response =
    updateUsing
        (\model -> dequeue model.requests)
        (\dequeuedRequests ->
            case dequeuedRequests of
                Just ( request, requests ) ->
                    updateSequence
                        [ updatePure (\model -> { model | requests = requests })
                        , case ( request, response ) of
                            ( RequestCreateItem request1, ResponseCreateItem result ) ->
                                handleResponseCreateItem request1 result

                            ( RequestCreateSection request1, ResponseCreateSection result ) ->
                                handleResponseCreateSection request1 result

                            ( RequestCreateStore request1, ResponseCreateStore result ) ->
                                handleResponseCreateStore request1 result

                            ( RequestDeleteItem request1, ResponseDeleteItem result ) ->
                                handleResponseDeleteItem request1 result

                            ( RequestDeleteSection storeId request1, ResponseDeleteSection _ result ) ->
                                handleResponseDeleteSection storeId request1 result

                            ( RequestDeleteStore request1, ResponseDeleteStore result ) ->
                                handleResponseDeleteStore request1 result

                            ( RequestGetItems _, ResponseGetItems result ) ->
                                handleResponseGetItems result

                            ( RequestItemInStore request1, ResponseItemInStore result ) ->
                                handleResponseItemInStore request1 result

                            ( RequestItemNotInStore request1, ResponseItemNotInStore result ) ->
                                handleResponseItemNotInStore request1 result

                            ( RequestMoveItemOff request1, ResponseMoveItemOff result ) ->
                                handleResponseMoveItemOff request1 result

                            ( RequestMoveItemOn request1, ResponseMoveItemOn result ) ->
                                handleResponseMoveItemOn request1 result

                            ( RequestRenameItem request1, ResponseRenameItem result ) ->
                                handleResponseRenameItem request1 result

                            ( RequestRenameSection request1, ResponseRenameSection result ) ->
                                handleResponseRenameSection request1 result

                            ( RequestRenameStore request1, ResponseRenameStore result ) ->
                                handleResponseRenameStore request1 result

                            ( RequestReorderSections request1, ResponseReorderSections result ) ->
                                handleResponseReorderSections request1 result

                            _ ->
                                updateNone
                        , fireOffNextRequest
                        ]

                Nothing ->
                    updateNone
        )


handleResponseCreateItem : CreateItemRequest -> Result Http.Error CreateItemResponse -> Update Msg Model
handleResponseCreateItem request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure
                    (\model ->
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
                    )
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseCreateSection : CreateSectionRequest -> Result Http.Error CreateSectionResponse -> Update Msg Model
handleResponseCreateSection request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure
                    (\model ->
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
                    )
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseCreateStore : CreateStoreRequest -> Result Http.Error CreateStoreResponse -> Update Msg Model
handleResponseCreateStore request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure
                    (\model ->
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
                    )
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseDeleteItem : DeleteItemRequest -> Result Http.Error DeleteItemResponse -> Update Msg Model
handleResponseDeleteItem request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure (assumeDeleteItemRequestWillSucceed request)
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseDeleteSection :
    StoreId
    -> DeleteSectionRequest
    -> Result Http.Error DeleteSectionResponse
    -> Update Msg Model
handleResponseDeleteSection storeId request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure (assumeDeleteSectionRequestWillSucceed storeId request)
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseDeleteStore : DeleteStoreRequest -> Result Http.Error DeleteStoreResponse -> Update Msg Model
handleResponseDeleteStore request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure (assumeDeleteStoreRequestWillSucceed request)
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseGetItems : Result Http.Error GetItemsResponse -> Update msg Model
handleResponseGetItems result =
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
            updatePure
                (\model ->
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
                )

        Err err ->
            case err of
                Http.BadStatus 304 ->
                    updateNone

                _ ->
                    updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseItemInStore : ItemInStoreRequest -> Result Http.Error ItemInStoreResponse -> Update Msg Model
handleResponseItemInStore request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure (assumeItemInStoreRequestWillSucceed request)
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseItemNotInStore : ItemNotInStoreRequest -> Result Http.Error ItemNotInStoreResponse -> Update Msg Model
handleResponseItemNotInStore request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure (assumeItemNotInStoreRequestWillSucceed request)
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseMoveItemOff : MoveItemOffRequest -> Result Http.Error MoveItemOffResponse -> Update Msg Model
handleResponseMoveItemOff request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure (assumeMoveItemOffRequestWillSucceed request)
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseMoveItemOn : MoveItemOnRequest -> Result Http.Error MoveItemOnResponse -> Update Msg Model
handleResponseMoveItemOn request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure (assumeMoveItemOnRequestWillSucceed request)
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseRenameItem : RenameItemRequest -> Result Http.Error RenameItemResponse -> Update Msg Model
handleResponseRenameItem request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure (assumeRenameItemRequestWillSucceed request)
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseRenameSection : RenameSectionRequest -> Result Http.Error RenameSectionResponse -> Update Msg Model
handleResponseRenameSection request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure (assumeRenameSectionRequestWillSucceed request)
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseRenameStore : RenameStoreRequest -> Result Http.Error RenameStoreResponse -> Update Msg Model
handleResponseRenameStore request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure (assumeRenameStoreRequestWillSucceed request)
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleResponseReorderSections : ReorderSectionsRequest -> Result Http.Error ReorderSectionsResponse -> Update Msg Model
handleResponseReorderSections request result =
    case result of
        Ok response ->
            updateSequence
                [ updatePure (assumeReorderSectionsRequestWillSucceed request)
                , fetchItemsIfOutOfDate response.dataVersion
                ]

        Err _ ->
            updatePure (\model -> { model | error = Just "Something went wrong." })


handleSetStorePageTab : StorePageTab -> Update msg Model
handleSetStorePageTab tab =
    updatePure
        (\model ->
            { model
                | reorderingSections = []
                , storePageTab = tab
            }
        )


handleShop : Update Msg Model
handleShop =
    updateUsing
        .shopping
        (\shopping ->
            replacePage
                (case shopping of
                    Nothing ->
                        ShoppingSelectionPage

                    Just storeId ->
                        ShoppingPage storeId
                )
        )


handleToggleDarkMode : Update Msg Model
handleToggleDarkMode =
    updateSequence
        [ updateCommand (\model -> saveModel (Encode.object [ ( "dark_mode", Encode.bool (not model.darkMode) ) ]))
        , updatePure (\model -> { model | darkMode = not model.darkMode })
        ]


handleUrlChanged : Url -> Update Msg Model
handleUrlChanged url =
    case Url.Parser.parse pageParser url of
        Just page ->
            updateSequence
                [ updatePure
                    (\model ->
                        { model
                            | page = page
                            , confirmingDelete = False
                            , fabValue = Nothing
                            , listPageAnimatingItems = Set.empty
                            , reorderingSections = []
                            , shopping =
                                case page of
                                    ShoppingPage storeId ->
                                        Just storeId

                                    ShoppingSelectionPage ->
                                        Nothing

                                    _ ->
                                        model.shopping
                        }
                    )
                , updateCommand_ (Task.perform (\_ -> Noop) (Browser.Dom.setViewport 0 0))
                ]

        Nothing ->
            updateNone


handleUrlRequested : Browser.UrlRequest -> Update Msg Model
handleUrlRequested urlRequest =
    case urlRequest of
        Browser.Internal url ->
            case Url.Parser.parse pageParser url of
                Just page ->
                    case page of
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

                Nothing ->
                    updateNone

        Browser.External href ->
            updateCommand_ (Navigation.load href)


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
                { onSubmit = CreateStore (Just itemId)
                , placeholder = "Star Market"
                , suggestions = []
                }

        ItemStorePage _ storeId ->
            viewSectionFab model storeId

        StorePage storeId ->
            case model.storePageTab of
                StorePageSectionsTab ->
                    viewSectionFab model storeId

                StorePageItemsTab ->
                    viewFabWith
                        model
                        { onSubmit = \name -> AddItemNameToListAndStore name storeId
                        , placeholder = "milk"
                        , suggestions = fabItemSuggestions model (\item -> AddItemIdToListAndStore item.id storeId)
                        }

        StoreItemPage storeId _ ->
            viewSectionFab model storeId

        -- TODO
        StoreSectionPage _ _ ->
            []

        -- TODO
        StoreSectionItemPage _ _ _ ->
            []

        StoresPage ->
            viewFabWith model
                { onSubmit = CreateStore Nothing
                , placeholder = "Star Market"
                , suggestions = []
                }

        ShoppingItemPage storeId _ ->
            viewSectionFab model storeId

        ShoppingPage _ ->
            viewListFab model

        ShoppingSelectionPage ->
            viewFabWith model
                { onSubmit = CreateStore Nothing
                , placeholder = "Star Market"
                , suggestions = []
                }


viewListFab : Model -> List (Html Msg)
viewListFab model =
    viewFabWith
        model
        { onSubmit = AddItemNameToList
        , placeholder = "milk"
        , suggestions = fabItemSuggestions model (\item -> AddItemIdToList item.id)
        }


viewSectionFab : Model -> StoreId -> List (Html Msg)
viewSectionFab model storeId =
    viewFabWith
        model
        { onSubmit = CreateSection storeId
        , placeholder = "Aisle 1"
        , suggestions = []
        }


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


type alias ViewFabInfo =
    { onSubmit : String -> Msg
    , placeholder : String
    , suggestions : List ( String, Msg )
    }


viewFabWith : Model -> ViewFabInfo -> List (Html Msg)
viewFabWith model info =
    [ case model.fabValue of
        Nothing ->
            text ""

        Just _ ->
            div [ class "scrim", onPointerup FabClose ] []
    , div
        [ class "fab" ]
        [ if List.isEmpty info.suggestions then
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
                    info.suggestions
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
            , placeholder info.placeholder
            , preventDefaultOn
                "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\key ->
                            case key of
                                "Enter" ->
                                    Decode.succeed ( info.onSubmit (Maybe.withDefault "" model.fabValue), True )

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

        onListItems : List Item
        onListItems =
            List.filter (\item -> item.onList || Set.member item.id model.listPageAnimatingItems) allItems

        offListItems : List Item
        offListItems =
            List.filter (\item -> not item.onList || Set.member item.id model.listPageAnimatingItems) allItems

        pointerEventHandlers : ItemId -> List (Html.Attribute Msg)
        pointerEventHandlers item =
            [ onPointercancel PointercancelItem
            , onPointerdown (PointerdownItem item)
            , onPointerup (PointerupItem item)
            ]

        animationClass : Bool -> Item -> String
        animationClass isUpperList item =
            if Set.member item.id model.listPageAnimatingItems then
                if item.onList == isUpperList then
                    if isUpperList then
                        "long-pressable arriving-from-below"

                    else
                        "long-pressable arriving-from-above"

                else if isUpperList then
                    "long-pressable departing-down"

                else
                    "long-pressable departing-up"

            else
                "long-pressable"

        viewOffListItem : Item -> Html Msg
        viewOffListItem item =
            li
                (class (animationClass False item) :: pointerEventHandlers item.id)
                [ div
                    [ class "muted" ]
                    [ text item.name
                    , smallMutedSymbol "add"
                    ]
                ]

        viewOnListItem : Item -> Html Msg
        viewOnListItem item =
            li
                (class (animationClass True item) :: pointerEventHandlers item.id)
                [ div [] [ text item.name ] ]
    in
    [ div
        [ class "page-header" ]
        [ smallMutedSymbol "list"
        , h1 [] [ text "List" ]
        ]
    , div
        [ class "page-content" ]
        [ onListItems
            |> List.sortBy (.name >> String.toLower)
            |> List.map viewOnListItem
            |> ul []
        , case offListItems of
            [] ->
                text ""

            _ ->
                let
                    allDeparting =
                        List.all (\item -> item.onList && Set.member item.id model.listPageAnimatingItems) offListItems
                in
                offListItems
                    |> List.sortBy (.name >> String.toLower)
                    |> List.map viewOffListItem
                    |> ul
                        (if allDeparting then
                            [ class "collapsing" ]

                         else
                            []
                        )
        , let
            numItems =
                Dict.size model.items
          in
          if numItems > 5 then
            text ""

          else if numItems > 0 then
            div
                [ class "tip" ]
                [ text "You can long-press an item to rename it, delete it, and more." ]

          else
            div
                [ class "tip" ]
                [ text "You can create a new item by clicking the + icon below." ]
        ]
    ]


viewSectionPage : Model -> Section -> List (Html Msg)
viewSectionPage model section =
    let
        storeName : String
        storeName =
            case Dict.get section.store model.stores of
                Just store ->
                    store.name

                Nothing ->
                    ""

        viewItem : Item -> Html msg
        viewItem item =
            li
                []
                [ Html.a
                    [ href (pageUrl (StoreSectionItemPage section.store section.id item.id)) ]
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
        , h1 [] [ editNameInput (Maybe.withDefault (storeName ++ ", " ++ section.name) model.editingName) ]
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
        |> ul [class "shopping-selection-page-stores"]
    , if Dict.isEmpty model.stores then
        div
            [ class "tip" ]
            [ text "Before you can shop, you have to create a store! You can create a new store by clicking the + icon below, or from the stores tab." ]

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
    , let
        tabHtml which s =
            button
                [ classList [ ( "active", model.storePageTab == which ) ]
                , onPointerup (SetStorePageTab which)
                ]
                [ text s ]
      in
      div
        [ class "tabs" ]
        [ tabHtml StorePageSectionsTab "Sections"
        , tabHtml StorePageItemsTab "Items"
        ]
    , case model.storePageTab of
        StorePageSectionsTab ->
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

                        sectionCounts : Dict SectionId Int
                        sectionCounts =
                            Dict.foldl
                                (\_ itemStores acc ->
                                    case Dict.get store.id itemStores of
                                        Just itemStore ->
                                            case itemStore.section of
                                                Just section ->
                                                    dictUpsert
                                                        section
                                                        (\maybeCount ->
                                                            case maybeCount of
                                                                Just count ->
                                                                    count + 1

                                                                Nothing ->
                                                                    1
                                                        )
                                                        acc

                                                Nothing ->
                                                    acc

                                        Nothing ->
                                            acc
                                )
                                Dict.empty
                                model.itemStores
                    in
                    if List.isEmpty sections then
                        div [ class "tip" ] [ text "You can create a new section by clicking the + icon below." ]

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
                                            , span
                                                [ class "li-label" ]
                                                [ text
                                                    (let
                                                        count =
                                                            Maybe.withDefault 0 (Dict.get section.id sectionCounts)
                                                     in
                                                     if count == 1 then
                                                        "1 item"

                                                     else
                                                        String.fromInt count ++ " items"
                                                    )
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

        StorePageItemsTab ->
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
                , onPointerup (PointerupSection (SoldHereInSection section.id))
                ]
                [ div [] [ text section.name ] ]
    in
    [ div
        [ class "page-header" ]
        [ button
            [ onPointerup ClickedBack ]
            [ smallMutedSymbol "arrow_back" ]
        , h1 [] [ text (item.name ++ " @ " ++ store.name) ]
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
                    , onPointerup (PointerupSection SoldHereSomewhere)
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
                    , onPointerup (PointerupSection NotSoldHere)
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
        div [ class "tip" ] [ text "You can create a new section by clicking the + icon below." ]

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
        , if Dict.isEmpty model.stores then
            div
                [ class "tip" ]
                [ text "You can create a new store by clicking the + icon below." ]

          else
            model.stores
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
    span [ class "li-label" ] [ text section.name ]


smallMutedSymbol : String -> Html msg
smallMutedSymbol name =
    span [ class "material-symbols-outlined icon-sm muted" ] [ text name ]


editNameInput : String -> Html Msg
editNameInput name =
    textarea
        [ autocomplete False
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


type Response
    = ResponseCreateItem (Result Http.Error CreateItemResponse)
    | ResponseCreateSection (Result Http.Error CreateSectionResponse)
    | ResponseCreateStore (Result Http.Error CreateStoreResponse)
    | ResponseDeleteItem (Result Http.Error DeleteItemResponse)
    | ResponseDeleteSection StoreId (Result Http.Error DeleteSectionResponse)
    | ResponseDeleteStore (Result Http.Error DeleteStoreResponse)
    | ResponseGetItems (Result Http.Error GetItemsResponse)
    | ResponseItemInStore (Result Http.Error ItemInStoreResponse)
    | ResponseItemNotInStore (Result Http.Error ItemNotInStoreResponse)
    | ResponseMoveItemOff (Result Http.Error MoveItemOffResponse)
    | ResponseMoveItemOn (Result Http.Error MoveItemOnResponse)
    | ResponseRenameItem (Result Http.Error RenameItemResponse)
    | ResponseRenameSection (Result Http.Error RenameSectionResponse)
    | ResponseRenameStore (Result Http.Error RenameStoreResponse)
    | ResponseReorderSections (Result Http.Error ReorderSectionsResponse)


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
                (Response << ResponseCreateItem)
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
                (Response << ResponseCreateSection)
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
                (Response << ResponseCreateStore)
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
                (Response << ResponseDeleteItem)
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
                (Response << ResponseDeleteSection storeId)
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
                (Response << ResponseDeleteStore)
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
                (Response << ResponseItemInStore)
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
                (Response << ResponseItemNotInStore)
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
                (Response << ResponseMoveItemOff)
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
                ]
    in
    Http.post
        { url = "/api/item-on"
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                (Response << ResponseMoveItemOn)
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
                (Response << ResponseRenameItem)
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
                (Response << ResponseRenameSection)
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
                (Response << ResponseReorderSections)
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
                (Response << ResponseRenameStore)
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
    }


assumeMoveItemOnRequestWillSucceed : MoveItemOnRequest -> Model -> Model
assumeMoveItemOnRequestWillSucceed request model =
    { model
        | items = dictUpdateIfExists request.item (\item -> { item | onList = True }) model.items
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


enqueueRequest : Request -> Update Msg Model
enqueueRequest request model =
    ( { model | requests = enqueue request model.requests }
    , -- If the queue was empty before enqueueing this request, send it.
      if queueIsEmpty model.requests then
        sendRequest request

      else
        Cmd.none
    )


enqueueRequestToFront : Request -> Update Msg Model
enqueueRequestToFront request =
    updateSequence
        [ updateUsing
            (\model -> queueIsEmpty model.requests)
            (\noInFlightRequests ->
                -- If the queue was empty before enqueueing this request, send it.
                if noInFlightRequests then
                    updateCommand_ (sendRequest request)

                else
                    updateNone
            )
        , updatePure (\model -> { model | requests = enqueueToFront request model.requests })
        ]


{-| Given a `data_version` from the server in response to a request that bumped it, if it appears a concurrent edit has
occurred (because the actual data version is more than 1 higher than the previous one), catch the client up by
enqueueing (to the front of the queue, before all other enqueued requests) a GET /items request.

Be careful to leave the model's dataVersion field alone if we're behind, so we send an old ETag along with our request
for the items.

-}
fetchItemsIfOutOfDate : Int -> Update Msg Model
fetchItemsIfOutOfDate actualDataVersion =
    updateUsing
        .dataVersion
        (\dataVersion ->
            if actualDataVersion == dataVersion + 1 then
                updatePure (\model -> { model | dataVersion = actualDataVersion })

            else
                enqueueRequestToFront (RequestGetItems { dataVersion = dataVersion })
        )


{-| Fire off the next request, if there is one.
-}
fireOffNextRequest : Update Msg Model
fireOffNextRequest =
    updateUsing
        (\model -> queuePeek model.requests)
        (\peekRequests ->
            case peekRequests of
                Nothing ->
                    updateNone

                Just ( request, requests ) ->
                    updateSequence
                        [ updatePure (\model -> { model | requests = requests })
                        , updateCommand_ (sendRequest request)
                        ]
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


pushPage : Page -> Update Msg Model
pushPage page =
    updateCommand (\model -> Navigation.pushUrl model.key (pageUrl page))


replacePage : Page -> Update Msg Model
replacePage page =
    updateCommand (\model -> Navigation.replaceUrl model.key (pageUrl page))



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
-- Update abstraction


type alias Update e a =
    a -> ( a, Cmd e )


updateNone : Update e a
updateNone x =
    ( x, Cmd.none )


updatePure : (a -> a) -> Update e a
updatePure f x =
    ( f x, Cmd.none )


updateCommand : (a -> Cmd e) -> Update e a
updateCommand f x =
    ( x, f x )


updateCommand_ : Cmd e -> Update e a
updateCommand_ e x =
    ( x, e )


updateUsing : (a -> b) -> (b -> Update e a) -> Update e a
updateUsing f g x =
    g (f x) x


updateSequence : List (Update e a) -> Update e a
updateSequence =
    let
        g : Update e a -> ( a, Cmd e ) -> ( a, Cmd e )
        g f ( model0, cmd0 ) =
            let
                ( model1, cmd1 ) =
                    f model0
            in
            ( model1, Cmd.batch [ cmd0, cmd1 ] )
    in
    List.foldl (\x f -> f >> g x) updateNone



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


isJust : Maybe a -> Bool
isJust x =
    case x of
        Nothing ->
            False

        Just _ ->
            True


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
