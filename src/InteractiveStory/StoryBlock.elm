module InteractiveStory.StoryBlock where

import Either exposing (Either)
import Time exposing (Time)
import Signal
import Effects exposing (Effects)

import Markdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)

import InteractiveStory.Action exposing (..)
import InteractiveStory.Trigger exposing (TriggerBuilder)
import InteractiveStory.VariableModel exposing (VariableModel)
import InteractiveStory.StoryBlockAction as SBAction

import InteractiveStory.Styles.Core exposing (storyBlock, animateIn, storyBlockAnimation)

import Animation


import Debug

---- STORYBLOCK MODEL ----

type alias Trigger = Either TriggerBuilder String
type alias AnimationState = { prevClockTime : Time, elapsedTime : Time }

type StoryBlock
    = ContentBlock
        { content            : String
        , variableEdits      : List VarAction
        , triggers           : List Trigger
        , disableProgression : Bool
        , label              : Maybe String
        , animationState     : Maybe AnimationState
        }
    | ChoiceBlock
        { queryText          : String
        , choices            : List Choice
        , variableEdits      : List VarAction
        , triggers           : List Trigger
        , label              : Maybe String
        , selection          : Maybe Int
        , animationState     : Maybe AnimationState
        }
    | CustomBlock
        { contentGenerator   : VariableModel -> Signal.Address Action -> Bool -> Html
        , genContent         : Signal.Address Action -> Bool -> Html
        , variableEdits      : List VarAction
        , triggers           : List Trigger
        , disableProgression : Bool
        , label              : Maybe String
        , animationState     : Maybe AnimationState
        }
    | LogicBlock
        { run                : VariableModel -> List Action
        , label              : Maybe String
        }
    | EndBlock
        { label              : Maybe String
        , triggers           : List Trigger
        , animationState     : Maybe AnimationState
        }

type alias Choice =
    { queryText             : String
    , jumpToLabel           : String
    , variableEdits         : List VarAction
    , triggerLiveVarUpdate  : Bool
    }

---- SOME HELPERS ----

getLabel : StoryBlock -> Maybe String
getLabel storyBlock =
    case storyBlock of
        ContentBlock { label } -> label
        ChoiceBlock  { label } -> label
        CustomBlock  { label } -> label
        LogicBlock   { label } -> label
        EndBlock     { label } -> label
        _ -> Nothing

getTriggers : StoryBlock -> List Trigger
getTriggers storyBlock =
    case storyBlock of
        ContentBlock { triggers } -> triggers
        ChoiceBlock  { triggers } -> triggers
        CustomBlock  { triggers } -> triggers
        EndBlock     { triggers } -> triggers
        _ -> []

getAnimationState : StoryBlock -> Maybe AnimationState
getAnimationState storyBlock =
    case storyBlock of
        ContentBlock { animationState } -> animationState
        ChoiceBlock  { animationState } -> animationState
        CustomBlock  { animationState } -> animationState
        EndBlock     { animationState } -> animationState
        _ -> Nothing

getDisableProgression : StoryBlock -> Bool
getDisableProgression storyBlock =
    case storyBlock of
        ContentBlock block -> block.disableProgression
        CustomBlock  block -> block.disableProgression
        _ -> False

getVariableEdits : StoryBlock -> List VarAction
getVariableEdits storyBlock =
    case storyBlock of
        ContentBlock { variableEdits } -> variableEdits
        ChoiceBlock  { variableEdits } -> variableEdits
        CustomBlock  { variableEdits } -> variableEdits
        _ -> []

mapDisableProgression : (Bool -> Bool) -> StoryBlock -> StoryBlock
mapDisableProgression f storyBlock =
    case storyBlock of
        ContentBlock block -> ContentBlock { block | disableProgression <- f block.disableProgression }
        CustomBlock  block -> CustomBlock  { block | disableProgression <- f block.disableProgression }
        _ -> storyBlock

animationInProgress : StoryBlock -> Bool
animationInProgress storyBlock =
    case getAnimationState storyBlock of
        Nothing -> False
        Just animationState ->
            Animation.isRunning animationState.elapsedTime storyBlockAnimation

---- DEFAULT MODELS ----

initialStoryBlock : StoryBlock
initialStoryBlock = ContentBlock { content = "", variableEdits = [], triggers = [], disableProgression = False, label = Nothing, animationState = Nothing }

emptyBlock : StoryBlock
emptyBlock = LogicBlock { run = always [], label = Nothing }

---- STORYBLOCK UPDATE ----

update : SBAction.Action -> StoryBlock -> (StoryBlock, Effects Action)
update action storyBlock =
    case action of
        SBAction.ChoiceSelect newSelection ->
            case storyBlock of
                ChoiceBlock block ->
                    (ChoiceBlock { block | selection <- newSelection }, Effects.none)
                _ -> (storyBlock, Effects.none)

        SBAction.AnimateIn ->
            case storyBlock of
                LogicBlock _ -> (storyBlock, Effects.none)
                _ ->
                    case getAnimationState storyBlock of
                        Nothing -> ( storyBlock, Effects.tick (StoryBlockAction << SBAction.Tick) )
                        Just _ -> ( storyBlock, Effects.none )


        SBAction.Tick clockTime ->
            case storyBlock of
                LogicBlock _ -> (storyBlock, Effects.none)
                _ ->
                    let (newAnimationState, newEffects) =
                        case getAnimationState storyBlock of
                            Nothing ->
                                let
                                    newAnimationState = { prevClockTime = clockTime, elapsedTime = 0 }
                                in (Just newAnimationState, Effects.tick (StoryBlockAction << SBAction.Tick))
                            Just { elapsedTime, prevClockTime } ->
                                let
                                    newElapsedTime = elapsedTime + (clockTime - prevClockTime)
                                    newAnimationState = { prevClockTime = clockTime, elapsedTime = newElapsedTime }
                                in
                                    if Animation.isDone newElapsedTime storyBlockAnimation
                                    then (Just newAnimationState, Effects.none)
                                    else (Just newAnimationState, Effects.tick (StoryBlockAction << SBAction.Tick))
                    in
                        case storyBlock of
                            ContentBlock block -> (ContentBlock { block | animationState <- newAnimationState }, newEffects)
                            ChoiceBlock  block -> (ChoiceBlock { block | animationState <- newAnimationState }, newEffects)
                            CustomBlock  block -> (CustomBlock { block | animationState <- newAnimationState }, newEffects)
                            EndBlock     block -> (EndBlock { block | animationState <- newAnimationState }, newEffects)



        _ -> (storyBlock, Effects.none)

---- STORYBLOCK RENDER ----

render : Signal.Address Action -> Bool -> StoryBlock -> Html
render address isActive block =
    let animationTime = (Maybe.withDefault { prevClockTime = 0, elapsedTime = 0 } <| getAnimationState block) |> .elapsedTime
    in
        case block of
            ContentBlock { content } ->
                div
                    [ makeBlockClassHeader "content-block" isActive, style <| animateIn animationTime <| storyBlock [] ]
                    [ Markdown.toHtml content ]

            ChoiceBlock { queryText, choices, selection } ->
                div
                    [ makeBlockClassHeader "choice-block" isActive, style <| animateIn animationTime <| storyBlock [] ]
                    [ Markdown.toHtml queryText
                    , choicesToHtmlList address isActive selection choices
                    ]

            CustomBlock { genContent } ->
                div
                    [ makeBlockClassHeader "custom-block" isActive, style <| animateIn animationTime <| storyBlock [] ]
                    [ genContent address isActive ]

            EndBlock _ ->
                div
                    [ makeBlockClassHeader "end-block" isActive, style <| animateIn animationTime <| storyBlock [] ]
                    [ text "End" ]

            LogicBlock _ -> div [ makeBlockClassHeader "logic-block" isActive, Attr.style [("visibility", "hidden"), ("max-height", "0px")] ] []

            _ -> div [ Attr.style [("display", "none")] ] []

makeClassList : List String -> List (String, Bool)
makeClassList classes =
    List.map (flip (,) True) classes

makeBlockClassHeader : String -> Bool -> Attribute
makeBlockClassHeader class isActive = Attr.classList <| ("active-block", isActive) :: makeClassList ["story-block", class]

choicesToHtmlList : Signal.Address Action -> Bool -> Maybe Int -> List Choice -> Html
choicesToHtmlList address isActive selection choices =
    let choiceToLi index { queryText, jumpToLabel, variableEdits, triggerLiveVarUpdate } =
        if isActive
        then
            let actionList =
                List.map (flip EditVar triggerLiveVarUpdate) variableEdits ++
                    [ StoryBlockAction (SBAction.ChoiceSelect (Just index))
                    , JumpToLabel jumpToLabel
                    ]
            in li [ onClick address <| Batch actionList ] [ text queryText ]
        else li [ class <| if Just index == selection then "selected-choice" else "", onClick address NoOp ] [ text queryText ]
    in ul [] <| List.indexedMap choiceToLi choices
