module VirtualList exposing
    ( Model, defaultConfig, init, initWithConfig, Msg, update
    , view
    , setItems, setItemsAndRemeasureAll, setItemsAndRemeasure
    , scrollToItem, Alignment(..)
    )

{-| Efficiently displays large lists by **only rendering the visible items** within the viewport, plus a configurable buffer.

It does so by **dynamically measuring** the height of the displayed elements.

In case you **know the heights in advance** you might get a better performance by using [`FabienHenon/elm-infinite-list-view`](https://package.elm-lang.org/packages/FabienHenon/elm-infinite-list-view/latest/).


## Usage


### Model Setup

Include `VirtualList.Model` in your app's **model:**

    import VirtualList

    type alias Model =
        { virtualList : VirtualList.Model

        -- other fields
        }

    defaultModel : Model
    defaultModel =
        { virtualList = VirtualList.init

        -- other fields
        }


### Update Function

Include `VirtualList.Msg` in your app’s **update function:**


    type Msg
        = VirtualListMsg VirtualList.Msg

    -- other messages
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            VirtualListMsg virtualListMsg ->
                let
                    ( virtualListModel, virtualListCmd ) =
                        VirtualList.update virtualListMsg model.virtualList
                in
                ( { model | virtualList = virtualListModel }, Cmd.map VirtualListMsg virtualListCmd )

    -- other cases


### Rendering

Render the virtual list in your **view:**

    view : Model -> Html Msg
    view model =
        VirtualList.view (renderRow model) model.virtualList VirtualListMsg

    renderRow : Model -> String -> Html Msg
    renderRow model id =
        div [] [ text id ]


# Model & Initialization

@docs Model, defaultConfig, init, initWithConfig, Msg, update


# Rendering

@docs view


# Updating the Items

@docs setItems, setItemsAndRemeasureAll, setItemsAndRemeasure


# Scrolling

@docs scrollToItem, Alignment

-}

import Browser.Dom
import Dict exposing (Dict, foldl)
import Html exposing (Html, div)
import Html.Attributes
import Html.Events exposing (on)
import Html.Lazy exposing (lazy2)
import Json.Decode as Decode
import List exposing (all)
import List.Extra
import Process
import Set exposing (Set)
import Task


showDebugLogs : Bool
showDebugLogs =
    True


log : String -> a -> a
log msg value =
    if showDebugLogs then
        Debug.log msg value

    else
        value


type alias Config =
    { listId : String
    , initialHeight : Float
    , defaultItemHeight : Float
    , showListDuringMeasurement : Bool
    , buffer : Int
    , dynamicBuffer : Bool
    }


{-| Provides a **default configuration** with sensible initial values.

  - **`listId`:** The ID of the list container in the DOM.
  - **`initialHeight`:** Estimated height of the list before it is measured.
  - **`defaultItemHeight`:** Default height assigned to items before they are measured.
  - **`showListDuringMeasurement`:** If `True`, the list is visible while measuring (potentially causing spacing issues).
  - **`buffer`:** Number of items rendered outside the visible range to ensure smooth scrolling.
  - **`dynamicBuffer`:** If `True`, increases buffer size when scrolling quickly.

```
defaultConfig : Config
defaultConfig =
    { listId = "virtual-list"
    , initialHeight = 500
    , defaultItemHeight = 26
    , showListDuringMeasurement = False
    , buffer = 5
    , dynamicBuffer = True
    }
```

-}
defaultConfig : Config
defaultConfig =
    { listId = "virtual-list"
    , initialHeight = 500
    , defaultItemHeight = 26
    , showListDuringMeasurement = False
    , buffer = 5
    , dynamicBuffer = True
    }


{-| `Model` for maintaining the **virtual list state.** You need to include it in your model:

    type alias Model =
        { virtualList : VirtualList.Model

        -- other fields
        }

You **create** one with the `init` function.

-}
type alias Model =
    { listId : String
    , itemIds : List String
    , listHeight : Float
    , defaultItemHeight : Float
    , baseBuffer : Int
    , dynamicBuffer : Bool
    , currentBuffer : Int
    , showListDuringMeasurement : Bool
    , listIsVisible : Bool
    , visibleRows : ( Int, Int )
    , unmeasuredRows : Set Int
    , rowHeights : Dict Int RowHeight
    , cumulativeRowHeights : Dict Int Float
    , scrollTop : Float
    , previousScrollTop : Float
    , scrollState : ScrollState
    }


{-| Initializes a virtual list model using the **default configuration.**
-}
init : Model
init =
    initWithConfig defaultConfig


{-| Initializes a virtual list model using a **custom configuration.**

    initWithConfig defaultConfig

You can **modify** the default configuration:

    config =
        { defaultConfig | buffer = 10 }

-}
initWithConfig : Config -> Model
initWithConfig options =
    let
        validListId =
            if String.isEmpty options.listId then
                defaultConfig.listId

            else
                options.listId

        validHeight =
            if options.initialHeight >= 0 then
                options.initialHeight

            else
                defaultConfig.initialHeight

        validBuffer =
            if options.buffer >= 0 then
                options.buffer

            else
                0

        validDefaultItemHeight =
            if options.defaultItemHeight >= 0 then
                options.defaultItemHeight

            else
                defaultConfig.defaultItemHeight

        estimatedVisibleCount =
            ceiling (validHeight / validDefaultItemHeight)

        initialVisibleRows =
            ( 0, estimatedVisibleCount + validBuffer )
    in
    { listId = validListId
    , itemIds = []
    , listHeight = validHeight
    , baseBuffer = validBuffer
    , dynamicBuffer = options.dynamicBuffer
    , currentBuffer = validBuffer
    , showListDuringMeasurement = options.showListDuringMeasurement
    , listIsVisible = options.showListDuringMeasurement
    , defaultItemHeight = validDefaultItemHeight
    , visibleRows = initialVisibleRows
    , unmeasuredRows = Set.empty
    , rowHeights = Dict.empty
    , cumulativeRowHeights = Dict.empty
    , scrollTop = 0
    , previousScrollTop = 0
    , scrollState = NoScroll
    }


type RowHeight
    = Unmeasured Float
    | Measured Float


{-| **Messages** handled by the virtual list.

You need to **include** `VirtualList.Msg` in your app’s `Msg` type and handle it in `update`.


    type Msg
        = VirtualListMsg VirtualList.Msg

    -- other messages

-}
type Msg
    = NoOp
    | RowElementReceived Int (Result Browser.Dom.Error Browser.Dom.Element)
    | Scrolled
    | ScrollStartRequested String Alignment
    | ViewportUpdated (Result Browser.Dom.Error Browser.Dom.Viewport)


{-| **Updates** the virtual list model in response to messages.

You must **integrate** this into your `update` function and map the result back to your own `Msg`.


    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            VirtualListMsg virtualListMsg ->
                let
                    ( virtualListModel, virtualListCmd ) =
                        VirtualList.update virtualListMsg model.virtualList
                in
                ( { model | virtualList = virtualListModel }, Cmd.map VirtualListMsg virtualListCmd )

    -- other cases

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RowElementReceived index result ->
            measureElementAndScroll model index result

        Scrolled ->
            updateBufferMeasureViewportAndContinueScroll model

        ScrollStartRequested id alignment ->
            let
                _ =
                    log "ScrollStartRequested" id
            in
            scrollToItem model id alignment

        ViewportUpdated result ->
            updateOnViewportChange model result


updateBufferMeasureViewportAndContinueScroll : Model -> ( Model, Cmd Msg )
updateBufferMeasureViewportAndContinueScroll model =
    let
        scrollSpeed =
            abs (model.scrollTop - model.previousScrollTop)

        newBuffer =
            if model.dynamicBuffer && model.scrollState == NoScroll then
                calculateDynamicBuffer model.baseBuffer scrollSpeed

            else
                model.currentBuffer

        ( newModel, scrollCmd ) =
            continueScrollToTarget model
    in
    ( { newModel | currentBuffer = newBuffer }
    , Cmd.batch
        [ measureViewport model.listId
        , scrollCmd
        ]
    )


scrollTargetToleranceInPixel : Float
scrollTargetToleranceInPixel =
    20


continueScrollToTarget : Model -> ( Model, Cmd Msg )
continueScrollToTarget model =
    case model.scrollState of
        InProgress scrollState ->
            let
                updatedScrollState =
                    updatePendingScrollWithNewMeasurements model scrollState
            in
            processScroll model updatedScrollState

        _ ->
            ( { model | listIsVisible = True }, Cmd.none )


processScroll : Model -> InProgressScrollState -> ( Model, Cmd Msg )
processScroll model scrollState =
    let
        newTargetOffset =
            scrollState.targetOffset

        lowerBound =
            model.scrollTop

        upperBound =
            model.scrollTop + model.listHeight

        isVisible =
            newTargetOffset >= lowerBound && newTargetOffset <= upperBound

        scrollOffset =
            abs (model.scrollTop - newTargetOffset)

        isClose =
            scrollOffset <= scrollTargetToleranceInPixel

        retryCount =
            case model.scrollState of
                SearchingForItem attempts ->
                    attempts

                _ ->
                    0

        newAttempts =
            retryCount + 1

        shouldRetry =
            newAttempts < maxScrollRetries

        stillTooFar =
            not isVisible && scrollOffset > 1.5 * scrollTargetToleranceInPixel

        logMessage =
            { scrollTop = model.scrollTop
            , targetOffset = newTargetOffset
            , upperBound = upperBound
            , height = model.listHeight
            , isClose = isClose
            , isVisible = isVisible
            , retries = retryCount
            }

        cmd =
            Task.perform (\_ -> Scrolled) (Process.sleep 50)
    in
    if isVisible || isClose then
        if scrollState.stableCount > 2 then
            log "✅ Scrolling Done" logMessage
                |> (\_ -> ( { model | scrollState = NoScroll, listIsVisible = True }, cmd ))

        else
            log "❓ Scrolling Might Be Done" logMessage
                |> (\_ ->
                        ( { model
                            | scrollState =
                                InProgress
                                    { scrollState
                                        | stableCount = scrollState.stableCount + 1
                                    }
                          }
                        , cmd
                        )
                   )

    else if shouldRetry && stillTooFar then
        log "🔄 Still Scrolling - Retrying Scroll" logMessage
            |> (\_ ->
                    ( model
                    , cmd
                    )
               )

    else
        log "🛑 Max retries reached, stopping scroll attempt." logMessage
            |> (\_ -> ( { model | scrollState = NoScroll, listIsVisible = True }, Cmd.none ))


maxScrollRetries : Int
maxScrollRetries =
    10


maxBufferMultiplier : Int
maxBufferMultiplier =
    4


calculateDynamicBuffer : Int -> Float -> Int
calculateDynamicBuffer base scrollSpeed =
    base * min maxBufferMultiplier (1 + round (scrollSpeed / 100))


{-| Updates the **list of items** displayed in the virtual list.

Only **new items** are considered for measurement; existing items retain their cached heights.

    VirtualList.setItems model.virtualList ids

-}
setItems : Model -> List String -> ( Model, Cmd Msg )
setItems model newIds =
    setItemsAndRemeasure model { newIds = newIds, idsToRemeasure = [] }


{-| Same as `setItems`, but allows **remeasuring** the entire list.

Use this when item heights may have **changed.**

    VirtualList.setItemsAndRemeasureAll model.virtualList newIds

-}
setItemsAndRemeasureAll : Model -> List String -> ( Model, Cmd Msg )
setItemsAndRemeasureAll model newIds =
    let
        ( newModel, cmd ) =
            setItemsAndRemeasure model { newIds = newIds, idsToRemeasure = newIds }
    in
    ( { newModel | listIsVisible = log "showing list from setItemsAndRemeasureAll" model.showListDuringMeasurement }, cmd )


{-| Same as `setItems`, but allows specifying which **items should be remeasured.**

This is useful when only a **subset of items** might have changed in height.

    VirtualList.setItemsAndRemeasure model.virtualList { newIds = newIds, idsToRemeasure = changedIds }

-}
setItemsAndRemeasure : Model -> { newIds : List String, idsToRemeasure : List String } -> ( Model, Cmd Msg )
setItemsAndRemeasure model { newIds, idsToRemeasure } =
    getRowHeightsFromCache { oldIds = model.itemIds, newIds = newIds, idsToRemeasure = idsToRemeasure } model.rowHeights model.defaultItemHeight
        |> updateModelWithNewItems model newIds


updateModelWithNewItems : Model -> List String -> Dict Int RowHeight -> ( Model, Cmd Msg )
updateModelWithNewItems model ids updatedRowHeights =
    ( { model
        | itemIds = ids
        , cumulativeRowHeights = calculateCumulativeRowHeights updatedRowHeights
        , rowHeights = updatedRowHeights
      }
    , measureViewport model.listId
    )


getRowHeightsFromCache :
    { oldIds : List String, newIds : List String, idsToRemeasure : List String }
    -> Dict Int RowHeight
    -- currentRowHeights (keyed by the old index)
    -> Float
    -- defaultItemHeight
    -> Dict Int RowHeight
getRowHeightsFromCache ids currentRowHeights defaultItemHeight =
    ids.newIds
        |> List.indexedMap (mapRowHeight ids currentRowHeights defaultItemHeight)
        |> Dict.fromList


mapRowHeight :
    { oldIds : List String, newIds : List String, idsToRemeasure : List String }
    -> Dict Int RowHeight
    -> Float
    -> Int
    -> String
    -> ( Int, RowHeight )
mapRowHeight { oldIds, idsToRemeasure } currentRowHeights defaultItemHeight newIndex id =
    let
        maybeOldIndex =
            findIndexForId oldIds id

        existingHeight =
            maybeOldIndex
                |> Maybe.andThen (\oldIndex -> Dict.get oldIndex currentRowHeights)

        newHeight =
            if List.member id idsToRemeasure then
                Unmeasured (Maybe.withDefault defaultItemHeight (Maybe.map rowHeightToFloat existingHeight))

            else
                Maybe.withDefault (Unmeasured defaultItemHeight) existingHeight
    in
    ( newIndex, newHeight )


findIndexForId : List String -> String -> Maybe Int
findIndexForId ids id =
    List.Extra.findIndex ((==) id) ids


calculateCumulativeRowHeights : Dict Int RowHeight -> Dict Int Float
calculateCumulativeRowHeights heights =
    foldl insertCumulativeHeight ( Dict.empty, 0 ) heights
        |> Tuple.first


insertCumulativeHeight : comparable -> RowHeight -> ( Dict comparable Float, Float ) -> ( Dict comparable Float, Float )
insertCumulativeHeight index rowHeight ( cumulativeHeights, cumulative ) =
    let
        height =
            rowHeightToFloat rowHeight

        cumulativeHeight =
            cumulative + height
    in
    ( Dict.insert index cumulativeHeight cumulativeHeights, cumulativeHeight )


rowHeightToFloat : RowHeight -> Float
rowHeightToFloat rowHeight =
    case rowHeight of
        Measured value ->
            value

        Unmeasured value ->
            value


calculateVisibleRows : Model -> Float -> Float -> ( Int, Int )
calculateVisibleRows model scrollTop containerHeight =
    let
        keys =
            Dict.keys model.cumulativeRowHeights

        itemCount =
            List.length model.itemIds

        height index =
            Maybe.withDefault model.defaultItemHeight (Dict.get index model.cumulativeRowHeights)

        start =
            keys
                |> List.Extra.find (\index -> height index >= scrollTop)
                |> Maybe.withDefault 0

        end =
            keys
                |> List.reverse
                |> List.Extra.find (\index -> height index < scrollTop + containerHeight)
                |> Maybe.withDefault (itemCount - 1)

        buffer =
            model.currentBuffer
    in
    ( max 0 (start - buffer), min itemCount (end + buffer) )


measureElementAndScroll : Model -> Int -> Result Browser.Dom.Error Browser.Dom.Element -> ( Model, Cmd Msg )
measureElementAndScroll model index result =
    case result of
        Ok element ->
            updateRowHeightAndScroll model index element

        Err error ->
            ( model, scrollCloserToTarget model error model.scrollState )


scrollCloserToTarget : Model -> Browser.Dom.Error -> ScrollState -> Cmd Msg
scrollCloserToTarget model error scrollState =
    case ( error, scrollState ) of
        ( Browser.Dom.NotFound _, InProgress inProgressScrollState ) ->
            scrollCmdForKnownTarget model (log "measureRowAndScroll, scrolling to index" inProgressScrollState.targetIndex) inProgressScrollState.alignment

        _ ->
            Cmd.none


updateRowHeightAndScroll : Model -> Int -> Browser.Dom.Element -> ( Model, Cmd Msg )
updateRowHeightAndScroll model index element =
    let
        height =
            element.element.height

        updatedRowHeights =
            Dict.insert index (Measured height) model.rowHeights

        updatedCumulativeHeights =
            calculateCumulativeRowHeights updatedRowHeights

        remainingUnmeasuredRows =
            Set.remove index model.unmeasuredRows

        newModel =
            { model
                | unmeasuredRows = remainingUnmeasuredRows
                , cumulativeRowHeights = updatedCumulativeHeights
                , rowHeights = updatedRowHeights
            }
                |> showListIfAllVisibleRowsAreMeasured
    in
    ( newModel
    , maybePendingScrollCmd newModel
    )


showListIfAllVisibleRowsAreMeasured : Model -> Model
showListIfAllVisibleRowsAreMeasured model =
    let
        ( start, end ) =
            model.visibleRows

        visibleIndices =
            List.range start (end - 1)

        unmeasuredVisibleRows =
            List.filter (\i -> isUnmeasured model.rowHeights i) visibleIndices

        _ =
            log "checkAndReveal" model.scrollState
    in
    if List.isEmpty unmeasuredVisibleRows && model.scrollState == NoScroll then
        { model | listIsVisible = True }

    else
        model


updateOnViewportChange : Model -> Result Browser.Dom.Error Browser.Dom.Viewport -> ( Model, Cmd Msg )
updateOnViewportChange model result =
    case result of
        Ok viewport ->
            measureVisibleRows model viewport

        Err _ ->
            ( model, Cmd.none )


measureVisibleRows : Model -> Browser.Dom.Viewport -> ( Model, Cmd Msg )
measureVisibleRows model viewport =
    let
        newScrollTop =
            viewport.viewport.y

        newListHeight =
            viewport.viewport.height

        (( start, end ) as visibleRows) =
            calculateVisibleRows model newScrollTop newListHeight

        unmeasuredIndices =
            List.range start (end - 1)
                |> List.filter (isUnmeasured model.rowHeights)

        measureCmds =
            unmeasuredIndices
                |> List.map requestRowMeasurement
                |> Cmd.batch
    in
    ( { model
        | listHeight = newListHeight
        , scrollTop = newScrollTop
        , previousScrollTop = model.scrollTop
        , visibleRows = visibleRows
        , unmeasuredRows = Set.fromList unmeasuredIndices
      }
    , measureCmds
    )


isUnmeasured : Dict comparable RowHeight -> comparable -> Bool
isUnmeasured rowHeights index =
    case Dict.get index rowHeights of
        Just (Unmeasured _) ->
            True

        Just (Measured _) ->
            False

        Nothing ->
            True


hideListAndRequestRowMeasurement : Model -> Int -> ( Model, Cmd Msg )
hideListAndRequestRowMeasurement model index =
    ( { model | listIsVisible = model.showListDuringMeasurement }, requestRowMeasurement index )


requestRowMeasurement : Int -> Cmd Msg
requestRowMeasurement index =
    Browser.Dom.getElement (rowId index)
        |> Task.attempt (RowElementReceived index)


rowId : Int -> String
rowId index =
    "virtual-list-item-" ++ String.fromInt index


{-| Defines where an item should **appear in the viewport** when scrolled to.


### Variants:

  - **`Top`:** Scrolls the item to the top.
  - **`Center`:** Scrolls the item to the center.
  - **`Bottom`:** Scrolls the item to the bottom.

-}
type Alignment
    = Top
    | Center
    | Bottom


type ScrollState
    = InProgress InProgressScrollState
    | NoScroll
    | SearchingForItem Int


type alias InProgressScrollState =
    { targetIndex : Int
    , alignment : Alignment
    , targetOffset : Float
    , stableCount : Int
    }


updatePendingScrollWithNewMeasurements : Model -> InProgressScrollState -> InProgressScrollState
updatePendingScrollWithNewMeasurements model pending =
    let
        newOffset =
            computeElementStart model pending.targetIndex

        delta =
            abs (newOffset - pending.targetOffset)
    in
    if delta > scrollTargetToleranceInPixel then
        { pending | targetOffset = newOffset }

    else
        pending


{-| Scrolls to the **specified item** in the virtual list.

Does nothing if the item is **already visible.**

    let
        ( newVirtualList, virtualListCmd ) =
            VirtualList.scrollToItem model.virtualList "item-42" VirtualList.Center
    in
    ( { model | virtualList = newVirtualList }
    , Cmd.map VirtualListMsg virtualListCmd
    )

-}
scrollToItem : Model -> String -> Alignment -> ( Model, Cmd Msg )
scrollToItem model id alignment =
    case findIndexForId model.itemIds id of
        Just index ->
            startScrollingToKnownItem model alignment (log "scrollToItem, found" index)

        Nothing ->
            let
                _ =
                    log "scrollToItem, not found" id
            in
            startScrollInNextUpdateCycle model id alignment


startScrollingToKnownItem : Model -> Alignment -> Int -> ( Model, Cmd Msg )
startScrollingToKnownItem model alignment index =
    let
        pendingScrollState =
            { targetIndex = index
            , alignment = alignment
            , targetOffset = 0
            , stableCount = 0
            }

        newModel =
            { model
                | scrollState = InProgress pendingScrollState
            }
    in
    scrollToKnownItem newModel pendingScrollState


scrollToKnownItem : Model -> InProgressScrollState -> ( Model, Cmd Msg )
scrollToKnownItem model scrollState =
    let
        rowIsMeasured =
            not <| isUnmeasured model.rowHeights scrollState.targetIndex

        computedOffset =
            computeElementStart model scrollState.targetIndex

        newScrollState =
            { scrollState | targetOffset = computedOffset }

        newModelPre =
            { model
                | scrollState = InProgress newScrollState
            }

        ( newModel, cmd ) =
            if log "rowIsMeasured" rowIsMeasured then
                ( newModelPre, scrollCmdForKnownTarget newModelPre newScrollState.targetIndex newScrollState.alignment )

            else
                hideListAndRequestRowMeasurement newModelPre scrollState.targetIndex
    in
    ( newModel, cmd )


startScrollInNextUpdateCycle : Model -> String -> Alignment -> ( Model, Cmd Msg )
startScrollInNextUpdateCycle model id alignment =
    case log "recheckScroll" model.scrollState of
        SearchingForItem attempts ->
            increaseAttemptsAndAttemptScrollInNextUpdateCycle model id alignment attempts

        InProgress _ ->
            ( model, Cmd.none )

        NoScroll ->
            increaseAttemptsAndAttemptScrollInNextUpdateCycle model id alignment 0


increaseAttemptsAndAttemptScrollInNextUpdateCycle : Model -> String -> Alignment -> Int -> ( Model, Cmd Msg )
increaseAttemptsAndAttemptScrollInNextUpdateCycle model id alignment attempts =
    if attempts < maxRecheckAttempts then
        let
            newAttempts =
                attempts + 1

            newModel =
                { model | scrollState = SearchingForItem newAttempts }
        in
        ( newModel, startScrollingInNextUpdateCycle id alignment )

    else
        -- Maximum attempts reached; clear pending scroll.
        ( { model | scrollState = NoScroll, listIsVisible = True }, Cmd.none )


startScrollingInNextUpdateCycle : String -> Alignment -> Cmd Msg
startScrollingInNextUpdateCycle id alignment =
    Task.perform (\_ -> ScrollStartRequested id alignment) (Process.sleep 100)


maxRecheckAttempts : Int
maxRecheckAttempts =
    1


maybePendingScrollCmd : Model -> Cmd Msg
maybePendingScrollCmd model =
    case model.scrollState of
        InProgress { targetIndex, alignment } ->
            if Set.isEmpty model.unmeasuredRows then
                scrollCmdForKnownTarget model (log "maybePendingScrollCmd" targetIndex) alignment

            else
                Cmd.none

        NoScroll ->
            Cmd.none

        SearchingForItem _ ->
            Cmd.none


scrollCmdForKnownTarget : Model -> Int -> Alignment -> Cmd Msg
scrollCmdForKnownTarget model index alignment =
    let
        elementStart =
            computeElementStart model index

        scrollNeeded =
            needsScrollCorrection model elementStart

        _ =
            log "scrollCmdForKnownTarget"
                { index = index
                , scrollNeeded = scrollNeeded
                }
    in
    if scrollNeeded then
        scrollToPosition
            { listId = model.listId
            , elementStart = elementStart
            , containerHeight = model.listHeight
            , nextElementStart = Dict.get index model.cumulativeRowHeights
            , alignment = alignment
            }

    else
        Task.perform (\_ -> Scrolled) (Process.sleep 0)


computeElementStart : Model -> Int -> Float
computeElementStart model index =
    case Dict.get (index - 1) model.cumulativeRowHeights of
        Just h ->
            h

        Nothing ->
            toFloat index * model.defaultItemHeight


needsScrollCorrection : Model -> Float -> Bool
needsScrollCorrection model targetOffset =
    abs (model.scrollTop - targetOffset) > 1


type alias ScrollPosition =
    { listId : String
    , elementStart : Float
    , containerHeight : Float
    , nextElementStart : Maybe Float
    , alignment : Alignment
    }


scrollToPosition : ScrollPosition -> Cmd Msg
scrollToPosition position =
    let
        nextElementStart =
            Maybe.withDefault position.elementStart position.nextElementStart

        finalPosition =
            case position.alignment of
                Top ->
                    position.elementStart

                Center ->
                    position.elementStart - 0.5 * position.containerHeight

                Bottom ->
                    nextElementStart - position.containerHeight
    in
    Cmd.batch
        [ Browser.Dom.setViewportOf position.listId 0 finalPosition
            |> Task.attempt (\_ -> NoOp)
        , Task.perform (\_ -> Scrolled) (Process.sleep 20) -- Make sure we're never stuck in the still need to scroll state
        ]



-- VIEW


{-| **Renders** the virtual list.

You **provide** it with

  - **function** that renders an item given its ID,
  - the virtual list **`Model`** and
  - the virtual list **message** type on your side.

In **your code** this would look like this:

    view : Model -> Html Msg
    view model =
        VirtualList.view (renderRow model) model.virtualList VirtualListMsg

    renderRow : Model -> String -> Html Msg
    renderRow model id =
        div [] [ text id ]

`renderRow` is executed **lazily.**

-}
view : (String -> Html msg) -> Model -> (Msg -> msg) -> Html msg
view renderRow model toSelf =
    let
        ( start, end ) =
            model.visibleRows

        visibleItems =
            slice start end model.itemIds

        height =
            String.fromFloat (totalHeight model.cumulativeRowHeights)

        rows =
            List.indexedMap
                (\localIndex id ->
                    let
                        globalIndex =
                            start + localIndex
                    in
                    renderRow id
                        |> renderLazyVirtualRow globalIndex model.cumulativeRowHeights
                )
                visibleItems
    in
    div
        (listAttributes model.listIsVisible model.listId toSelf)
        [ renderSpacer height rows ]


listAttributes : Bool -> String -> (Msg -> msg) -> List (Html.Attribute msg)
listAttributes showList listId toSelf =
    [ Html.Attributes.class "virtual-list"
    , Html.Attributes.id listId

    -- Height needs to be in the element for fast measurement
    , Html.Attributes.style "height" "100%"
    , Html.Attributes.style "overflow" "auto"
    , onScroll (toSelf Scrolled)
    ]
        ++ (if not showList then
                [ Html.Attributes.style "visibility" "hidden" ]

            else
                []
           )


totalHeight : Dict Int Float -> Float
totalHeight cumulativeHeights =
    let
        lastItemIndex =
            Dict.size cumulativeHeights - 1
    in
    case Dict.get lastItemIndex cumulativeHeights of
        Just height ->
            height

        Nothing ->
            0


renderSpacer : String -> List (Html msg) -> Html msg
renderSpacer height rows =
    div
        [ Html.Attributes.style "height" (height ++ "px")
        , Html.Attributes.style "position" "relative"
        ]
        rows


renderLazyVirtualRow : Int -> Dict Int Float -> Html msg -> Html msg
renderLazyVirtualRow index cumulativeHeights renderRow =
    let
        top =
            Maybe.withDefault 0 (Dict.get (index - 1) cumulativeHeights)

        id =
            rowId index
    in
    lazy2 (renderVirtualRow renderRow) id top


renderVirtualRow : Html msg -> String -> Float -> Html msg
renderVirtualRow renderRow id top =
    div
        [ Html.Attributes.id id
        , Html.Attributes.style "transform" ("translateY(" ++ String.fromFloat top ++ "px)")
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.class "virtual-list-item"
        ]
        [ renderRow ]


slice : Int -> Int -> List a -> List a
slice start end list =
    list
        |> List.drop start
        |> List.take (end - start)


onScroll : msg -> Html.Attribute msg
onScroll msg =
    on "scroll" (Decode.succeed msg)


measureViewport : String -> Cmd Msg
measureViewport listId =
    Task.attempt ViewportUpdated (Browser.Dom.getViewportOf listId)
