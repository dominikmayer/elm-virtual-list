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
import List
import List.Extra
import Measurable exposing (Measurable(..))
import Process
import Set exposing (Set)
import Task



-- CONSTANTS


maxBufferMultiplier : Int
maxBufferMultiplier =
    4


maxScrollRecheckAttempts : Int
maxScrollRecheckAttempts =
    1


maxScrollRetries : Int
maxScrollRetries =
    10


scrollStabilityThreshold : Int
scrollStabilityThreshold =
    2


scrollTargetToleranceInPixel : Float
scrollTargetToleranceInPixel =
    20


showDebugLogs : Bool
showDebugLogs =
    False


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
    { itemIds : List String
    , currentBuffer : Int
    , listIsVisible : Bool
    , visibleRows : ( Int, Int )
    , unmeasuredRows : Set Int
    , rowHeights : Dict Int RowHeight
    , cumulativeRowHeights : Dict Int Float
    , scrollState : ScrollState
    , settings : Settings
    , viewport : Measurable Viewport
    }


type alias Settings =
    { listId : String
    , defaultItemHeight : Float
    , baseBuffer : Int
    , dynamicBuffer : Bool
    , showListDuringMeasurement : Bool
    }


type ScrollState
    = InProgress InProgressScrollState
    | ManualScroll
    | NoScroll
    | SearchingForItem Int


type alias InProgressScrollState =
    { targetIndex : Int
    , alignment : Alignment
    , targetOffset : Float
    , stableCount : Int
    }


type alias Viewport =
    { height : Float
    , previousScrollTop : Float
    , scrollTop : Float
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
        settings =
            validateConfig options

        validHeight =
            ensurePositiveOr defaultConfig.initialHeight options.initialHeight

        estimatedVisibleCount =
            ceiling (validHeight / settings.defaultItemHeight)

        initialVisibleRows =
            ( 0, estimatedVisibleCount + settings.baseBuffer )
    in
    { itemIds = []
    , currentBuffer = settings.baseBuffer
    , listIsVisible = options.showListDuringMeasurement
    , visibleRows = initialVisibleRows
    , unmeasuredRows = Set.empty
    , rowHeights = Dict.empty
    , cumulativeRowHeights = Dict.empty
    , settings = settings
    , scrollState = NoScroll
    , viewport =
        Unmeasured
            { height = validHeight
            , previousScrollTop = 0
            , scrollTop = 0
            }
    }


validateConfig : Config -> Settings
validateConfig config =
    { listId =
        if String.isEmpty config.listId then
            defaultConfig.listId

        else
            config.listId
    , defaultItemHeight = ensurePositiveOr defaultConfig.defaultItemHeight config.defaultItemHeight
    , baseBuffer = ensurePositive config.buffer
    , dynamicBuffer = config.dynamicBuffer
    , showListDuringMeasurement = config.showListDuringMeasurement
    }


ensurePositive : number -> number
ensurePositive number =
    ensurePositiveOr 0 number


ensurePositiveOr : number -> number -> number
ensurePositiveOr fallback number =
    if number >= 0 then
        number

    else
        fallback


type alias RowHeight =
    Measurable Float


{-| **Messages** handled by the virtual list.

You need to **include** `VirtualList.Msg` in your app’s `Msg` type and handle it in `update`.


    type Msg
        = VirtualListMsg VirtualList.Msg

    -- other messages

-}
type Msg
    = NoOp
    | RowElementReceived Int (Result Browser.Dom.Error Browser.Dom.Element)
    | Scrolled Float
    | ScrollRecheckRequested
    | ScrollStartRequested String Alignment
    | ScrollStopCheckRequested Float
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

        Scrolled scrollTop ->
            let
                ( newModel, measureCmd ) =
                    updateScroll model scrollTop
                        |> measureVisibleRows
            in
            ( newModel
            , Cmd.batch
                [ measureCmd
                , Task.perform (\_ -> ScrollStopCheckRequested scrollTop) (Process.sleep 200)
                ]
            )

        ScrollRecheckRequested ->
            continueScrollToTarget model

        ScrollStartRequested id alignment ->
            let
                _ =
                    log "ScrollStartRequested" id
            in
            scrollToItem model id alignment

        ScrollStopCheckRequested scrollTop ->
            let
                viewport =
                    Measurable.value model.viewport
            in
            if scrollTop == viewport.scrollTop then
                ( { model | scrollState = log "scrollRecheck" NoScroll }, Cmd.none )

            else
                ( model, Cmd.none )

        ViewportUpdated result ->
            handleViewportChange model result


updateScroll : Model -> Float -> Model
updateScroll model newScrollTop =
    let
        _ =
            log "updateScroll" model.scrollState

        newModel =
            if model.scrollState == NoScroll then
                { model | scrollState = ManualScroll }

            else
                model
    in
    updateScrollTop newModel newScrollTop
        |> updateBuffer


updateScrollTop : Model -> Float -> Model
updateScrollTop model newScrollTop =
    { model | viewport = setScrollTop model.viewport newScrollTop }


setScrollTop : Measurable Viewport -> Float -> Measurable Viewport
setScrollTop measurableViewport newScrollTop =
    case measurableViewport of
        Unmeasured viewport ->
            Unmeasured { viewport | previousScrollTop = viewport.scrollTop, scrollTop = newScrollTop }

        Measured viewport ->
            Measured { viewport | previousScrollTop = viewport.scrollTop, scrollTop = newScrollTop }


updateBuffer : Model -> Model
updateBuffer model =
    { model | currentBuffer = calculateBuffer model }


calculateBuffer : Model -> Int
calculateBuffer model =
    if model.settings.dynamicBuffer && model.scrollState == ManualScroll then
        Measurable.value model.viewport
            |> calculateDynamicBuffer model.settings.baseBuffer

    else
        model.currentBuffer


calculateDynamicBuffer : Int -> Viewport -> Int
calculateDynamicBuffer base viewport =
    base * min maxBufferMultiplier (1 + round (scrollSpeed viewport / 100))


scrollSpeed : Viewport -> Float
scrollSpeed viewport =
    abs (viewport.scrollTop - viewport.previousScrollTop)


continueScrollToTarget : Model -> ( Model, Cmd Msg )
continueScrollToTarget model =
    case model.scrollState of
        InProgress scrollState ->
            let
                updatedScrollState =
                    updateScrollStateWithNewMeasurements model scrollState
            in
            processScroll model updatedScrollState

        ManualScroll ->
            ( model, Cmd.none )

        SearchingForItem _ ->
            ( model, Cmd.none )

        NoScroll ->
            ( { model | listIsVisible = True }, Cmd.none )


processScroll : Model -> InProgressScrollState -> ( Model, Cmd Msg )
processScroll model scrollState =
    let
        viewport =
            Measurable.value model.viewport

        newTargetOffset =
            scrollState.targetOffset

        lowerBound =
            viewport.scrollTop

        upperBound =
            viewport.scrollTop + viewport.height

        isVisible =
            newTargetOffset >= lowerBound && newTargetOffset <= upperBound

        scrollOffset =
            abs (viewport.scrollTop - newTargetOffset)

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
            { scrollTop = viewport.scrollTop
            , targetOffset = newTargetOffset
            , upperBound = upperBound
            , height = viewport.height
            , isClose = isClose
            , isVisible = isVisible
            , retries = retryCount
            }

        cmd =
            Task.perform (\_ -> ScrollRecheckRequested) (Process.sleep 50)
    in
    if isVisible || isClose then
        if scrollState.stableCount >= scrollStabilityThreshold then
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
    setItemsAndRemeasure model { newIds = newIds, idsToRemeasure = newIds }


{-| Same as `setItems`, but allows specifying which **items should be remeasured.**

This is useful when only a **subset of items** might have changed in height.

    VirtualList.setItemsAndRemeasure model.virtualList { newIds = newIds, idsToRemeasure = changedIds }

-}
setItemsAndRemeasure : Model -> { newIds : List String, idsToRemeasure : List String } -> ( Model, Cmd Msg )
setItemsAndRemeasure model { newIds, idsToRemeasure } =
    let
        ( newModel, cmd ) =
            getRowHeightsFromCache
                { oldIds = model.itemIds
                , newIds = newIds
                , idsToRemeasure = idsToRemeasure
                }
                model.rowHeights
                model.settings.defaultItemHeight
                |> updateModelWithNewItems model newIds
    in
    ( newModel |> setListVisibility, cmd )


updateModelWithNewItems : Model -> List String -> Dict Int RowHeight -> ( Model, Cmd Msg )
updateModelWithNewItems model ids updatedRowHeights =
    ( { model
        | itemIds = ids
        , cumulativeRowHeights = calculateCumulativeRowHeights updatedRowHeights
        , rowHeights = updatedRowHeights
      }
    , measureViewport (log "measureViewport from updateModelWithNewItems" model.settings.listId)
    )


getRowHeightsFromCache :
    { oldIds : List String, newIds : List String, idsToRemeasure : List String }
    -> Dict Int RowHeight
    -> Float
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
                Unmeasured (Maybe.withDefault defaultItemHeight (Maybe.map Measurable.value existingHeight))

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
            Measurable.value rowHeight

        cumulativeHeight =
            cumulative + height
    in
    ( Dict.insert index cumulativeHeight cumulativeHeights, cumulativeHeight )


calculateVisibleRows : Model -> ( Int, Int )
calculateVisibleRows model =
    let
        viewport =
            Measurable.value model.viewport

        keys =
            Dict.keys model.cumulativeRowHeights

        itemCount =
            List.length model.itemIds

        height index =
            Maybe.withDefault model.settings.defaultItemHeight (Dict.get index model.cumulativeRowHeights)

        start =
            keys
                |> List.Extra.find (\index -> height index >= viewport.scrollTop)
                |> Maybe.withDefault 0

        end =
            keys
                |> List.reverse
                |> List.Extra.find (\index -> height index < viewport.scrollTop + viewport.height)
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
            ( model |> setListVisibility, scrollCloserToTarget model error model.scrollState )


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

        cmd =
            if Set.isEmpty newModel.unmeasuredRows then
                maybePendingScrollCmd newModel

            else
                Cmd.none
    in
    ( newModel |> setListVisibility
    , cmd
    )


handleViewportChange : Model -> Result Browser.Dom.Error Browser.Dom.Viewport -> ( Model, Cmd Msg )
handleViewportChange model result =
    case result of
        Ok viewport ->
            -- if log "viewport changed" (viewportChanged model.viewport viewport) || log "has unmeasured rows" (not (Set.isEmpty model.unmeasuredRows)) then
            updateViewport model viewport
                |> measureVisibleRows

        -- else
        --     ( model, Cmd.none )
        Err _ ->
            ( model, Cmd.none )


updateViewport : Model -> Browser.Dom.Viewport -> Model
updateViewport model domViewport =
    let
        oldViewport =
            Measurable.value model.viewport
    in
    { model
        | viewport =
            Measured
                { oldViewport
                    | height = domViewport.viewport.height
                    , previousScrollTop = oldViewport.scrollTop
                    , scrollTop = domViewport.viewport.y
                }
    }



-- viewportChanged : Viewport -> Browser.Dom.Viewport -> Bool
-- viewportChanged oldViewport viewport =
--     oldViewport.height /= viewport.viewport.height || oldViewport.scrollTop /= viewport.viewport.y


measureVisibleRows : Model -> ( Model, Cmd Msg )
measureVisibleRows model =
    let
        (( start, end ) as visibleRows) =
            calculateVisibleRows model

        unmeasuredIndices =
            List.range start (end - 1)
                |> List.filter (isUnmeasured model.rowHeights)

        measureCmds =
            unmeasuredIndices
                |> List.map requestRowMeasurement
                |> Cmd.batch

        newModel =
            { model
                | visibleRows = visibleRows
                , unmeasuredRows = Set.fromList unmeasuredIndices
            }
                |> setListVisibility
    in
    ( newModel, measureCmds )


setListVisibility : Model -> Model
setListVisibility model =
    if model.listIsVisible then
        { model | listIsVisible = log "should show list" (shouldShowList model) }

    else
        model


shouldShowList : Model -> Bool
shouldShowList model =
    if
        model.settings.showListDuringMeasurement
            || (log "scroll" model.scrollState == ManualScroll)
    then
        True

    else
        log "unmeasured items" (unmeasuredItemsInVisibleArea model.visibleRows model.rowHeights)


unmeasuredItemsInVisibleArea : ( Int, Int ) -> Dict Int RowHeight -> Bool
unmeasuredItemsInVisibleArea visibleRows rowHeights =
    let
        ( start, end ) =
            visibleRows

        visibleIndices =
            List.range start (end - 1)

        unmeasuredCount =
            visibleIndices
                |> List.filter (isUnmeasured rowHeights)
                |> List.length
    in
    unmeasuredCount <= 1


isUnmeasured : Dict comparable RowHeight -> comparable -> Bool
isUnmeasured rowHeights index =
    case Dict.get index rowHeights of
        Just (Unmeasured _) ->
            True

        Just (Measured _) ->
            False

        Nothing ->
            True


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


updateScrollStateWithNewMeasurements : Model -> InProgressScrollState -> InProgressScrollState
updateScrollStateWithNewMeasurements model scrollState =
    let
        newOffset =
            computeElementStart model scrollState.targetIndex

        delta =
            abs (newOffset - scrollState.targetOffset)
    in
    if delta > scrollTargetToleranceInPixel then
        { scrollState | targetOffset = newOffset }

    else
        scrollState


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
                ( newModelPre, requestRowMeasurement scrollState.targetIndex )
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

        ManualScroll ->
            increaseAttemptsAndAttemptScrollInNextUpdateCycle model id alignment 0


increaseAttemptsAndAttemptScrollInNextUpdateCycle : Model -> String -> Alignment -> Int -> ( Model, Cmd Msg )
increaseAttemptsAndAttemptScrollInNextUpdateCycle model id alignment attempts =
    if attempts < maxScrollRecheckAttempts then
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


maybePendingScrollCmd : Model -> Cmd Msg
maybePendingScrollCmd model =
    case model.scrollState of
        InProgress { targetIndex, alignment } ->
            scrollCmdForKnownTarget model (log "maybePendingScrollCmd" targetIndex) alignment

        NoScroll ->
            Cmd.none

        SearchingForItem _ ->
            Cmd.none

        ManualScroll ->
            Cmd.none


scrollCmdForKnownTarget : Model -> Int -> Alignment -> Cmd Msg
scrollCmdForKnownTarget model index alignment =
    let
        elementStart =
            computeElementStart model index

        scrollNeeded =
            needsScrollCorrection model elementStart

        containerHeight =
            Measurable.value model.viewport |> .height

        _ =
            log "scrollCmdForKnownTarget"
                { index = index
                , scrollNeeded = scrollNeeded
                }
    in
    if scrollNeeded then
        scrollToPosition
            { listId = model.settings.listId
            , elementStart = elementStart
            , containerHeight = containerHeight
            , nextElementStart = Dict.get index model.cumulativeRowHeights
            , alignment = alignment
            }

    else
        Task.perform (\_ -> ScrollRecheckRequested) (Task.succeed ())


computeElementStart : Model -> Int -> Float
computeElementStart model index =
    case Dict.get (index - 1) model.cumulativeRowHeights of
        Just h ->
            h

        Nothing ->
            toFloat index * model.settings.defaultItemHeight


needsScrollCorrection : Model -> Float -> Bool
needsScrollCorrection model targetOffset =
    let
        viewport =
            Measurable.value model.viewport
    in
    abs (viewport.scrollTop - targetOffset) > 1


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
        , Task.perform (\_ -> ScrollRecheckRequested) (Process.sleep 20) -- Make sure we're never stuck in the still need to scroll state
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
        height =
            String.fromFloat (totalHeight model.cumulativeRowHeights)

        rows =
            renderRows model renderRow
    in
    div
        (listAttributes model.listIsVisible model.settings.listId toSelf)
        [ renderSpacer height rows ]


renderRows : Model -> (String -> Html msg) -> List (Html msg)
renderRows model renderRow =
    let
        ( start, end ) =
            model.visibleRows

        visibleItems =
            slice start end model.itemIds
    in
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


listAttributes : Bool -> String -> (Msg -> msg) -> List (Html.Attribute msg)
listAttributes showList listId toSelf =
    [ Html.Attributes.class "virtual-list"
    , Html.Attributes.id listId

    -- Height needs to be in the element for fast measurement
    , Html.Attributes.style "height" "100%"
    , Html.Attributes.style "overflow" "auto"
    , onScroll (toSelf << Scrolled)
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


onScroll : (Float -> msg) -> Html.Attribute msg
onScroll toMsg =
    on "scroll" (Decode.map toMsg (Decode.at [ "target", "scrollTop" ] Decode.float))


measureViewport : String -> Cmd Msg
measureViewport listId =
    Task.attempt ViewportUpdated (Browser.Dom.getViewportOf listId)
