module InteractiveStory.Core where

import SelectionList as SL
import SelectionList exposing (SelectionList)

import InteractiveStory.StoryBlock exposing (..)
import InteractiveStory.StoryBlock as SB
import InteractiveStory.StoryBlockAction as SBAction
import InteractiveStory.Trigger exposing (..)
import InteractiveStory.Action exposing (..)
import InteractiveStory.VariableModel exposing (..)
import Effects exposing (Effects)

import DOMInterface

import Html.Events.Extra exposing (onScroll)

import Either

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy
import Maybe
import Signal

import Debug
import Task exposing (andThen)

import Dict exposing (Dict)
import String
import Regex

import InteractiveStory.Styles.Core exposing (fullscreen, topBar, fixed, spacer)

import Animation exposing (Animation)

import Html.Events.Extra exposing (ScrollEvent)

import List.Extra exposing (takeWhile)


---- INTERACTIVE STORY MODEL ----

type alias Model = {
    storyTrack           : SelectionList StoryBlock,
    currentBlockInstance : StoryBlock,
    blockHistory         : List StoryBlock,
    chunkStack           : List Chunk,
    chunking             : Bool,
    vars                 : VariableModel,
    windowHeight         : Int,
    windowWidth          : Int,
    scrollData           : ScrollEvent,
    scrollAnimation      : AnimationState,
    chunkSize            : Int,
    visibleChunks        : Int,
    chunkingThreshold    : Int
}

type alias AnimationState = { prevClockTime : Float, elapsedTime : Float, animation : Animation }
type alias Chunk = { height : Float, blocks : List StoryBlock }

init : List StoryBlock -> (Model, Effects Action)
init inputList =
    let head = List.head inputList |> Maybe.withDefault SB.emptyBlock
        tail = List.tail inputList |> Maybe.withDefault []
    in progressToNewBlockWith identity True
        { storyTrack = SL.fromList head tail
        , currentBlockInstance = head
        , blockHistory = []
        , chunkStack = []
        , chunking = False
        , vars = { string = Dict.empty, num =  Dict.empty, bool = Dict.empty }
        , windowWidth = 0
        , windowHeight = 0
        , scrollData = { scrollTop = 0, scrollLeft = 0, scrollWidth = 0, scrollHeight = 0, clientWidth = 0, clientHeight = 0 }
        , scrollAnimation = { prevClockTime = 0, elapsedTime = 0, animation = Animation.static 0 }
        , chunkSize = 10
        , visibleChunks = 0
        , chunkingThreshold = 5
        }

---- INTERACTIVE STORY UPDATE ----

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        NextBlock ->
            nextBlock model

        JumpToLabel label ->
            jumpToLabel label model

        Trigger action triggerSourceIndex ->
            trigger model action triggerSourceIndex

        EnableProgression ->
            let newInstanceBlock = SB.mapDisableProgression (always False) model.currentBlockInstance
            in ({ model | currentBlockInstance <- newInstanceBlock }, Effects.none)

        EditVar varAction liveUpdate ->
            let vars = model.vars
                newVars =
                    case varAction of
                        SetString key value ->
                            { vars | string <- Dict.insert key value model.vars.string }

                        SetNum key value ->
                            { vars | num <- Dict.insert key value model.vars.num }

                        SetBool key value ->
                            { vars | bool <- Dict.insert key value model.vars.bool }

                        UpdateString key f ->
                            { vars | string <- Dict.update key f model.vars.string }

                        UpdateNum key f ->
                            { vars | num <- Dict.update key f model.vars.num }

                        UpdateBool key f ->
                            { vars | bool <- Dict.update key f model.vars.bool }

            in ({ model | vars <- newVars }, Effects.none)
                |> (if liveUpdate then (stringReplaceVariables >> handleCustomBlocks) else identity)

        StoryBlockAction sbAction ->
            let (newInstanceBlock, newEffects) = SB.update sbAction model.currentBlockInstance
            in ({ model | currentBlockInstance <- newInstanceBlock }, newEffects)

        WindowResize (width, height) ->
            ({ model | windowHeight <- height, windowWidth <- width }, Effects.none)

        UserScroll scrollEvent ->
            let newVisibleChunks =
                List.foldl
                    (\{ height } (heightAcc, sum) ->
                        if height + heightAcc >= toFloat scrollEvent.scrollTop
                        then (height + heightAcc, sum + 1)
                        else (height + heightAcc, sum)
                    )
                    (0,0)
                    (List.reverse model.chunkStack)
                |> snd
            in
                ({ model | scrollData <- scrollEvent, visibleChunks <- newVisibleChunks }, Effects.none)

        AnimateScroll animation ->
            if Animation.isDone model.scrollAnimation.elapsedTime model.scrollAnimation.animation
            then
                ({ model |
                    scrollAnimation <- { prevClockTime = -1, elapsedTime = 0, animation = animation }
                }, Effects.tick Tick)
            else (model, Effects.none)

        Tick clockTime ->
            if Animation.isDone model.scrollAnimation.elapsedTime model.scrollAnimation.animation
            then (model, Effects.none)
            else
                let
                    currentScrollAnimation = model.scrollAnimation
                    newScrollAnimation =
                        if model.scrollAnimation.prevClockTime < 0
                        then { currentScrollAnimation | prevClockTime <- clockTime }
                        else
                            { currentScrollAnimation |
                                prevClockTime <- clockTime,
                                elapsedTime <- currentScrollAnimation.elapsedTime + (clockTime - currentScrollAnimation.prevClockTime)
                            }
                    scrollEffect =
                        DOMInterface.scrollElementTo
                            (0, round <| Animation.animate newScrollAnimation.elapsedTime newScrollAnimation.animation)
                            ".interactive-story-container" 0
                        |> Task.toMaybe
                        |> Task.map (always NoOp)
                        |> Effects.task

                in ({ model | scrollAnimation <- newScrollAnimation }, Effects.batch [scrollEffect, Effects.tick Tick])

        ApplyChunking height ->
            let newChunk = { height = height, blocks = List.take model.chunkSize model.blockHistory }
                newHistory = List.drop model.chunkSize model.blockHistory
            in ({ model | chunkStack <- newChunk :: model.chunkStack, blockHistory <- newHistory, chunking <- False }, Effects.none)

        Batch actions ->
            let foldFn action' (model', effects) =
                    let (newModel, newEffects) = update action' model'
                    in (newModel, Effects.batch [effects, newEffects])
            in List.foldl foldFn (model, Effects.none) actions

        _ -> (model, Effects.none)

---- UPDATE FUNCTIONS ----

nextBlock : Model -> (Model, Effects Action)
nextBlock = progressToNewBlockWith moveTrackForward False

jumpToLabel : String -> Model -> (Model, Effects Action)
jumpToLabel label = progressToNewBlockWith (flip jumpTo label) False

trigger : Model -> Action -> Maybe Int -> (Model, Effects Action)
trigger model action triggerSourceIndex =
    if Just (SL.selectedIndex model.storyTrack) == triggerSourceIndex || triggerSourceIndex == Nothing
    then update action model
    else (model, Effects.none)

---- ACTION COMPONENT FUNCTIONS ----
---- These functions are meant to be easily composable to simplify code

progressToNewBlockWith : ((Model, Effects Action) -> (Model, Effects Action)) -> Bool -> Model -> (Model, Effects Action)
progressToNewBlockWith progressFn allowRepeats model =
    (model, Effects.none)
    |> progressFn
    |> applyVariableEdits
    |> stringReplaceVariables
    |> applyTriggers
    |> handleCustomBlocks
    |> handleLogicBlocks
    |> animateBlockIn
    |> applyChunking
    |> (\(model, effects) ->
        case model.currentBlockInstance of
            LogicBlock _ -> (model, effects)
            _ -> addAdditionalEffects [scrollToLast model] (model, effects)
        )
    |> (if not allowRepeats then removeRepeatBlocks model else identity)

animateBlockIn : (Model, Effects Action) -> (Model, Effects Action)
animateBlockIn (model, effects) =
    case model.currentBlockInstance of
        LogicBlock _ -> (model, effects)
        _            ->
            let (newStoryBlock, effects') = SB.update SBAction.AnimateIn model.currentBlockInstance
            in ({ model | currentBlockInstance <- newStoryBlock }, Effects.batch [effects, effects'])

removeRepeatBlocks : Model -> (Model, Effects Action) -> (Model, Effects Action)
removeRepeatBlocks oldModel (newModel, effects) =
    if SL.selectedIndex newModel.storyTrack == SL.selectedIndex oldModel.storyTrack
        || SB.animationInProgress oldModel.currentBlockInstance
    then (oldModel, Effects.none)
    else (newModel, effects)

moveTrackForward : (Model, Effects Action) -> (Model, Effects Action)
moveTrackForward (model, effects) =
    if SB.getDisableProgression model.currentBlockInstance
    then (model, effects)
    else
        let newTrack = SL.next model.storyTrack
            newCurrentBlockInstance = newTrack.selected
            newHistory = model.blockHistory ++ [ model.currentBlockInstance ]

            returnVal =
                ({ model | storyTrack <- newTrack,
                      currentBlockInstance <- newCurrentBlockInstance,
                      blockHistory <- newHistory }
                , effects)
        in case model.currentBlockInstance of
            ContentBlock _ -> returnVal
            CustomBlock  _ -> returnVal
            LogicBlock   _ -> returnVal
            _ -> (model, effects)

applyVariableEdits : (Model, Effects Action) -> (Model, Effects Action)
applyVariableEdits (model, effects) =
    SB.getVariableEdits model.currentBlockInstance
    |> List.map (flip EditVar True)
    |> Batch
    |> flip update model
    |> (\(model', effects') -> (model', Effects.batch [effects, effects']))

applyTriggers : (Model, Effects Action) -> (Model, Effects Action)
applyTriggers (model, effects) =
    let triggers =
        partitionTriggers model.currentBlockInstance
        |> fst
        |> List.map (\f -> f <| SL.selectedIndex model.storyTrack)
    in (model, Effects.batch <| effects :: triggers)

jumpTo : (Model, Effects Action) -> (String -> (Model, Effects Action))
jumpTo (model, effects) =
    \label ->
        case getIndexOfLabel label model of
            Just index ->
                let newTrack   = SL.goto index model.storyTrack
                    newCurrentBlockInstance = newTrack.selected
                    newHistory = model.blockHistory ++ [ model.currentBlockInstance ]
                in ({ model | storyTrack <- newTrack,
                              currentBlockInstance <- newCurrentBlockInstance,
                              blockHistory <- newHistory }
                    , effects)

            Nothing -> (model, effects)

stringReplaceVariables : (Model, Effects Action) -> (Model, Effects Action)
stringReplaceVariables (model, effects) =
    let regex = Regex.regex "\\{\\{.*?\\}\\}" |> Regex.caseInsensitive
        replaceFn { match } =
            match
            |> String.dropLeft 2
            |> String.dropRight 2
            |> (\str ->
                Maybe.oneOf
                    (Dict.get str model.vars.string ::
                        (Maybe.map toString << Dict.get str) model.vars.num ::
                            (Maybe.map toString << Dict.get str) model.vars.bool ::
                                []
                    )
                )
            |> Maybe.withDefault match
        injectVars = Regex.replace Regex.All regex replaceFn
    in case (model.storyTrack.selected, model.currentBlockInstance) of
        (ContentBlock baseBlock, ContentBlock instanceBlock) ->
            ({ model | currentBlockInstance <- ContentBlock { instanceBlock | content <- injectVars baseBlock.content }}, effects)
        (ChoiceBlock baseBlock, ChoiceBlock instanceBlock) ->
            let newChoices =
                baseBlock.choices |> List.map (\choice -> { choice | queryText <- injectVars choice.queryText, jumpToLabel <- injectVars choice.jumpToLabel })
                newQueryText = injectVars baseBlock.queryText
            in ({ model | currentBlockInstance <- ChoiceBlock { instanceBlock | queryText <- newQueryText, choices <- newChoices} }, effects)
        _ -> (model, effects)

handleLogicBlocks : (Model, Effects Action) -> (Model, Effects Action)
handleLogicBlocks (model, effects) =
    case model.currentBlockInstance of
        LogicBlock { run } -> update (Batch <| run model.vars) model
        _ -> (model, effects)

handleCustomBlocks : (Model, Effects Action) -> (Model, Effects Action)
handleCustomBlocks (model, effects) =
    case model.currentBlockInstance of
        CustomBlock block  ->
            let newInstanceBlock = CustomBlock { block | genContent <- block.contentGenerator model.vars }
            in ({ model | currentBlockInstance <- newInstanceBlock }, effects)
        _ -> (model, effects)

addAdditionalEffects : List (Effects Action) -> (Model, Effects Action) -> (Model, Effects Action)
addAdditionalEffects extraEffects (model, effects) =
    (model, Effects.batch <| effects :: extraEffects)

applyChunking : (Model, Effects Action) -> (Model, Effects Action)
applyChunking (model, effects) =
    let threshold = model.chunkingThreshold
        chunkSize = model.chunkSize
        chunkItUp =
            (DOMInterface.getElementPositionInfo ".story-block" 1
            `andThen` \startElem -> DOMInterface.getElementPositionInfo ".story-block" chunkSize
            `andThen` \lastElem -> Task.succeed (lastElem.top + lastElem.height - startElem.top))
            |> Task.toMaybe
            |> Task.map (Maybe.map ApplyChunking)
            |> Task.map (Maybe.withDefault NoOp)
            |> Effects.task
    in
        if List.length model.blockHistory >= chunkSize + threshold && not model.chunking
        then addAdditionalEffects [chunkItUp] ({ model | chunking <- True }, effects)
        else (model, effects)

---- HELPER FUNCTIONS ----

getIndexOfLabel : String -> Model -> Maybe Int
getIndexOfLabel label model =
    let
        getIndexOfLabel' label blockList index =
            case blockList of
                (x::xs) -> if SB.getLabel x == label then Just index else getIndexOfLabel' label xs (index+1)
                [] -> Nothing
    in getIndexOfLabel' (Just label) (SL.toList model.storyTrack) 0

partitionTriggers : StoryBlock -> (List (Int -> Effects Action), List String)
partitionTriggers storyBlock
    = SB.getTriggers storyBlock
    |> List.partition Either.isLeft
    |> (\(ls, rs) ->
        ( List.filterMap (Either.elim (Just << identity) (always Nothing)) ls
        , List.filterMap (Either.elim (always Nothing) (Just << identity)) rs
        )
    )

scrollToLast : Model -> Effects Action
scrollToLast model =
    DOMInterface.getElementPositionInfo ".story-block" (-1)
    |> Task.toMaybe
    |> Task.map (\result ->
        case result of
            Nothing -> NoOp
            Just {top, height} ->
                Animation.animation 0
                |> Animation.from (toFloat model.scrollData.scrollTop)
                |> Animation.to (toFloat <| model.scrollData.scrollTop + (round top) - 100)
                |> Animation.duration 500
                |> AnimateScroll
        )
    |> Effects.task



---- INTERACTIVE STORY VIEW ----

render : Signal.Address Action -> Model -> Html
render address model =
    let (chunksBeforeViewport, chunksWithinViewport, chunksAfterViewport) = partitionChunks model model.chunkStack
    in Html.Lazy.lazy2
        div
            [ class "interactive-story-container", style <| fullscreen [], onScroll address UserScroll ]
           ([ div
                [ style <| topBar model.scrollData model.windowWidth [] ]
                [ div
                    [ style <| fixed <| topBar model.scrollData model.windowWidth [] ]
                    []
                ]
            , div
                [
                    style <| spacer (round <| getChunkBlockHeight chunksBeforeViewport)  []
                ] []
            ]
            ++
            ( chunksWithinViewport
                |> List.map .blocks
                |> List.concat
                |> List.map (Html.Lazy.lazy3 SB.render address False)
            )
            ++
            [ div
                [
                    style <| spacer (round <| getChunkBlockHeight chunksAfterViewport)  []
                ] []
            ]
            ++
            (List.map
                (Html.Lazy.lazy3 SB.render address False)
                model.blockHistory
                ++ [SB.render address True model.currentBlockInstance]
            )
            ++
            [ div [style <| spacer model.windowHeight []] [] ]
           )

getChunkBlockHeight : List Chunk -> Float
getChunkBlockHeight =
    List.foldl (\{ height } sum -> sum + height) 0

partitionChunks : Model -> List Chunk -> (List Chunk, List Chunk, List Chunk)
partitionChunks model chunks =
    let
        chunksTopToBottom = List.reverse chunks
        chunksWithTop =
            List.foldl
                (\{ height, blocks } (newChunkList, currentTop) ->
                    ( { top = currentTop, height = height, blocks = blocks } :: newChunkList, currentTop + height )
                )
                ([], 0)
                chunksTopToBottom
            |> fst
            |> List.reverse -- put chunks back in top-to-bottom order

        viewportPrerenderMargin = (toFloat model.scrollData.clientHeight) * 0.10

        viewportTop = toFloat model.scrollData.scrollTop - viewportPrerenderMargin
        viewportBottom = (toFloat <| model.scrollData.scrollTop + model.scrollData.clientHeight) + viewportPrerenderMargin

        collidesWithViewport {top, height} =
            top >= viewportTop && top <= viewportBottom
            || (top + height) >= viewportTop && (top + height) <= viewportBottom
            || top <= viewportTop && (top + height) >= viewportBottom

        (collidingChunks, nonCollidingChunks) = List.partition collidesWithViewport chunksWithTop

        comesBeforeViewport {top, height} = (top + height) <= viewportTop

        (beforeViewport, afterViewport) = List.partition comesBeforeViewport nonCollidingChunks

        toChunks = List.map (\{height, blocks} -> { height = height, blocks = blocks })
    in (toChunks beforeViewport, toChunks collidingChunks, toChunks afterViewport)
