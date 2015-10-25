module InteractiveStory.Core where

import SelectionList as SL
import SelectionList exposing (SelectionList)

import InteractiveStory.StoryBlock exposing (..)
import InteractiveStory.StoryBlock as SB
import InteractiveStory.StoryBlockAction as SBAction
-- import InteractiveStory.Trigger exposing (..)
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

import InteractiveStory.Sound as Sound

import InteractiveStory.Styles.Core exposing (fullscreen, topBar, fixed, spacer, topBarAnimationFrom)

import Animation exposing (Animation)
import AnimationWrapper as AW
import Howler

import Html.Events.Extra exposing (ScrollEvent)

import List.Extra exposing (takeWhile)



---- INTERACTIVE STORY MODEL ----

type alias Model = {
    storyTrack           : SelectionList StoryBlock,
    soundModel           : Sound.SoundModel,
    queuedEffectSet      : List EffectSet,
    blockHistory         : List (Signal.Address Action -> Html),
    chunkStack           : List Chunk,
    chunking             : Bool,
    vars                 : VariableModel,
    windowHeight         : Int,
    windowWidth          : Int,
    scrollData           : ScrollEvent,
    scrollAnimation      : AW.AnimationWrapper,
    chunkSize            : Int,
    chunkingThreshold    : Int
}

type alias AnimationState = { prevClockTime : Float, elapsedTime : Float, animation : Animation }
type alias Chunk = { height : Float, blocks : List (Signal.Address Action -> Html) }

init : List StoryBlock -> List (String, Howler.AudioObject) -> (Model, Effects Action)
init inputList audioList =
    let head = List.head inputList |> Maybe.withDefault SB.emptyStoryBlock
        tail = List.tail inputList |> Maybe.withDefault []
        emptyVarModel = { string = Dict.empty, num =  Dict.empty, bool = Dict.empty }
        preloadAudio =
            List.map (\(soundLabel, audioObj) -> Howler.create soundLabel audioObj) audioList
            |> Task.sequence
            |> Task.toMaybe
            |> Task.map (always NoOp)
            |> Effects.task
    in progressToNewBlockWith (always identity) True
        { storyTrack = SL.fromList head tail
        , soundModel = Sound.emptySoundModel
        , blockHistory = []
        , chunkStack = []
        , chunking = False
        , queuedEffectSet = []
        , vars = emptyVarModel
        , windowWidth = 0
        , windowHeight = 0
        , scrollData = { scrollTop = 0, scrollLeft = 0, scrollWidth = 0, scrollHeight = 0, clientWidth = 0, clientHeight = 0 }
        , scrollAnimation = AW.empty
        , chunkSize = 10
        , chunkingThreshold = 5
        }
        |> \(model, effects) -> (model, Effects.batch [preloadAudio, effects])

---- INTERACTIVE STORY UPDATE ----

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        NextBlock ->
            nextBlock model

        JumpToLabel label ->
            jumpToLabel label model

        SoundAction soundAction ->
            let (newSoundModel, newSoundEffects) = Sound.update soundAction model.soundModel
            in ({ model | soundModel <- newSoundModel }, Effects.map SoundAction newSoundEffects)

        RunEffectSet effectSet ->
            handleEffectSet (always effectSet) (model, Effects.none)

        RunEffectSetBeforeLeave effectSet ->
            ({ model | queuedEffectSet <- effectSet :: model.queuedEffectSet }, Effects.none)

        EditVar varAction ->
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

        StoryBlockAction sbAction ->
            let (newInstanceBlock, newEffects) = SB.update sbAction model.storyTrack.selected
                newStoryTrack = SL.updateSelected (always newInstanceBlock) model.storyTrack
            in ({ model | storyTrack <- newStoryTrack }, Effects.map StoryBlockAction newEffects)

        WindowResize (width, height) ->
            ({ model | windowHeight <- height, windowWidth <- width }, Effects.none)

        UserScroll scrollEvent ->
            ({ model | scrollData <- scrollEvent }, Effects.none)

        AnimateScroll animation ->
            let (newScrollAnimation, newEffects) = AW.update (AW.Start animation) AW.empty
            in
                (,)
                    { model | scrollAnimation <- newScrollAnimation }
                    (Effects.map ScrollTick newEffects)

        ScrollTick scrollAnimAction ->
            let (newScrollAnimation, newEffects) = AW.update scrollAnimAction model.scrollAnimation
                scrollEffect =
                        DOMInterface.scrollElementTo
                            (0, round <| AW.value newScrollAnimation)
                            ".interactive-story-container"
                        |> Task.toMaybe
                        |> Task.map (always NoOp)
                        |> Effects.task
            in
                (,)
                    { model | scrollAnimation <- newScrollAnimation }
                    ( Effects.batch [scrollEffect, Effects.map ScrollTick newEffects] )

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
jumpToLabel label = progressToNewBlockWith (jumpTo label) False

---- ACTION COMPONENT FUNCTIONS ----
---- These functions are meant to be easily composable to simplify code

progressToNewBlockWith : (VariableModel -> (Model, Effects Action) -> (Model, Effects Action)) -> Bool -> Model -> (Model, Effects Action)
progressToNewBlockWith progressFn initialRun model =
    (model, Effects.none)
    |> skipIfInitialRun initialRun addToHistory
    |> processQueuedEffectSets
    |> skipIfInitialRun initialRun (handleEffectSet (\m -> m.storyTrack.selected.onLeave m.vars))
    |> progressFn model.vars -- decide on progression with the last set of vars since onLeave should be after transitioning
    |> (handleEffectSet (\m -> m.storyTrack.selected.onEnter m.vars))
    |> initStoryBlock
    |> animateBlockIn
    |> applyChunking
    |> skipIfInitialRun initialRun scrollToBlock
    |> skipIfInitialRun initialRun (removeRepeatBlocks model)

skipIfInitialRun initialRun f = if initialRun then identity else f

processQueuedEffectSets : (Model, Effects Action) -> (Model, Effects Action)
processQueuedEffectSets (model, effects) =
    let effectSets = List.reverse model.queuedEffectSet
        (newModel, newEffects) =
            List.foldl
                (\effectSet newTuple -> handleEffectSet (always effectSet) newTuple)
                (model, effects)
                effectSets
        newModel' = { newModel | queuedEffectSet <- [] }
    in (newModel', newEffects)

initStoryBlock : (Model, Effects Action) -> (Model, Effects Action)
initStoryBlock (model, effects) =
    let (newBlock, newEffects) = SB.update (SBAction.Init model.vars) model.storyTrack.selected
        newStoryTrack = SL.updateSelected (always newBlock) model.storyTrack
    in ({ model | storyTrack <- newStoryTrack }, Effects.batch [effects, Effects.map StoryBlockAction newEffects])

addToHistory : (Model, Effects Action) -> (Model, Effects Action)
addToHistory (model, effects) =
    let newHistoricalBlock = (\addr -> SB.render addr False model.vars model.storyTrack.selected)
    in ( { model | blockHistory <- model.blockHistory ++ [newHistoricalBlock] }, effects )

handleEffectSet : (Model -> EffectSet) -> (Model, Effects Action) -> (Model, Effects Action)
handleEffectSet getEffectSet (model, effects) =
    let { variableEdits, soundUpdates } = getEffectSet model
    in
        (model, effects)
        |> applyVariableEdits variableEdits
        |> applySounds soundUpdates

applySounds : List Sound.SoundAction -> (Model, Effects Action) -> (Model, Effects Action)
applySounds soundActions (model, effects) =
    soundActions
    |> Debug.log "soundActions"
    |> List.map SoundAction
    |> Batch
    |> flip update model
    |> (\(model', effects') -> (model', Effects.batch [effects, effects']))


applyVariableEdits : List VarAction -> (Model, Effects Action) -> (Model, Effects Action)
applyVariableEdits variableEdits (model, effects) =
    variableEdits
    |> Debug.log "varEdits"
    |> List.map EditVar
    |> Batch
    |> flip update model
    |> (\(model', effects') -> (model', Effects.batch [effects, effects']))

scrollToBlock : (Model, Effects Action) -> (Model, Effects Action)
scrollToBlock (model, effects) =
    addAdditionalEffects [scrollToLast model] (model, effects)

animateBlockIn : (Model, Effects Action) -> (Model, Effects Action)
animateBlockIn (model, effects) =
    let (newStoryBlock, effects') = SB.update SBAction.AnimateIn model.storyTrack.selected
        newStoryTrack = SL.updateSelected (always newStoryBlock) model.storyTrack
    in ({ model | storyTrack <- newStoryTrack }, Effects.batch [effects, Effects.map StoryBlockAction effects'])

removeRepeatBlocks : Model -> (Model, Effects Action) -> (Model, Effects Action)
removeRepeatBlocks oldModel (newModel, effects) =
    if  SL.selectedIndex newModel.storyTrack == SL.selectedIndex oldModel.storyTrack ||
        SB.animationInProgress oldModel.storyTrack.selected
    then (oldModel, Effects.none)
    else (newModel, effects)

moveTrackForward : VariableModel -> (Model, Effects Action) -> (Model, Effects Action)
moveTrackForward vars (model, effects) =
    case model.storyTrack.selected.next vars of
        Stop -> (model, effects)
        Continue -> ( { model | storyTrack <- SL.next model.storyTrack }, effects)
        Label label -> jumpTo label vars (model, effects)

jumpTo : String -> VariableModel -> (Model, Effects Action) -> (Model, Effects Action)
jumpTo label _ (model, effects) =
    getIndexOfLabel label model
    `Maybe.andThen` (\index ->
        Just ({ model | storyTrack <- SL.goto index model.storyTrack }, effects)
    ) |> Maybe.withDefault (model, effects)

addAdditionalEffects : List (Effects Action) -> (Model, Effects Action) -> (Model, Effects Action)
addAdditionalEffects extraEffects (model, effects) =
    (model, Effects.batch <| effects :: extraEffects)

applyChunking : (Model, Effects Action) -> (Model, Effects Action)
applyChunking (model, effects) =
    let threshold = model.chunkingThreshold
        chunkSize = model.chunkSize
        chunkItUp =
            (DOMInterface.getElementPositionInfo ".story-block"
            `andThen` (List.head >> Task.fromMaybe DOMInterface.NodeUndefined)
            `andThen` \startElem ->
                DOMInterface.getElementPositionInfo ".story-block"
                `andThen` (List.drop (chunkSize) >> List.head >> Task.fromMaybe DOMInterface.NodeUndefined)
                `andThen` \lastElem ->
                    Task.succeed (
                        (lastElem.top - lastElem.margin.top) - (startElem.top - startElem.margin.top)
                    )
            )
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
    let getIndexOfLabel' label blockList index =
            case blockList of
                (x::xs) -> if x.label == label then Just index else getIndexOfLabel' label xs (index+1)
                [] -> Nothing
    in getIndexOfLabel' (Just label) (SL.toList model.storyTrack) 0

scrollToLast : Model -> Effects Action
scrollToLast model =
    Task.sleep 50
    `andThen` (\_ -> DOMInterface.getElementPositionInfo ".story-block")
    `andThen` (List.reverse >> List.head >> Task.fromMaybe DOMInterface.NodeUndefined)
    |> Task.toMaybe
    |> Task.map (\result ->
        case result of
            Nothing -> NoOp
            Just {top, height} ->
                Animation.animation 0
                |> Animation.from (toFloat model.scrollData.scrollTop)
                |> Animation.to (toFloat <| model.scrollData.scrollTop + (round top) - (round <| toFloat model.windowHeight * 0.2))
                |> Animation.duration 500
                |> AnimateScroll
        )
    |> Effects.task



---- INTERACTIVE STORY VIEW ----

render : Signal.Address Action -> Model -> Html
render address model =
    let (chunksBeforeViewport, chunksWithinViewport, chunksAfterViewport) = partitionChunks model
        scrollData = model.scrollData
    in Html.Lazy.lazy2
        div
            [ class "interactive-story-container", style <| fullscreen [], onScroll address UserScroll ]
           ([ div
                [ style <| topBar { scrollData | scrollTop <- 0 } model.windowWidth [] ]
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
                |> List.map (\historyRender -> Html.Lazy.lazy historyRender address)
            )
            ++
            [ div
                [
                    style <| spacer (round <| getChunkBlockHeight chunksAfterViewport)  []
                ] []
            ]
            ++
            (
                List.map (\historyRender -> Html.Lazy.lazy historyRender address) model.blockHistory
                ++
                [SB.render address True model.vars model.storyTrack.selected]
            )
            ++
            [ div [style <| spacer model.windowHeight []] [] ]
           )

getChunkBlockHeight : List Chunk -> Float
getChunkBlockHeight =
    List.foldl (\{ height } sum -> sum + height) 0

partitionChunks : Model -> (List Chunk, List Chunk, List Chunk)
partitionChunks model =
    let
        chunksTopToBottom = List.reverse model.chunkStack
        chunksWithTop =
            List.foldl
                (\{ height, blocks } (newChunkList, currentTop) ->
                    ( { top = currentTop, height = height, blocks = blocks } :: newChunkList, currentTop + height )
                )
                ([], topBarAnimationFrom )
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
