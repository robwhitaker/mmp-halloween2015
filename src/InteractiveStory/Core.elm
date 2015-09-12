module InteractiveStory.Core where

import SelectionList as SL
import SelectionList exposing (SelectionList)

import InteractiveStory.StoryBlock exposing (..)
import InteractiveStory.StoryBlock as SB
import InteractiveStory.Trigger exposing (..)
import InteractiveStory.Action exposing (..)
import Effects exposing (Effects)

import Either

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Maybe
import Signal

import StoryContent

---- INTERACTIVE STORY MODEL ----

type alias Model = {
    storyTrack       : SelectionList StoryBlock,
    blockHistory     : List StoryBlock
}

initialModel : Model
initialModel = { storyTrack = SL.fromList StoryContent.first StoryContent.stuff
               , blockHistory = []
               }

---- INTERACTIVE STORY UPDATE ----

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        NextBlock ->
            nextBlock model

        JumpToLabel label fromChoiceId ->
            jumpToLabel model label fromChoiceId

        Trigger action triggerSourceIndex ->
            trigger model action triggerSourceIndex

        _ -> (model, Effects.none)

---- UPDATE FUNCTIONS ----

nextBlock : Model -> (Model, Effects Action)
nextBlock model = model |> flip (,) Effects.none >> moveTrackForward >> applyTriggers |> removeRepeatBlocks model

jumpToLabel : Model -> String -> Int -> (Model, Effects Action)
jumpToLabel model label fromChoiceId = jumpTo (model, Effects.none) label fromChoiceId |> applyTriggers |> removeRepeatBlocks model

trigger : Model -> Action -> Int -> (Model, Effects Action)
trigger model action triggerSourceIndex =
    if SL.selectedIndex model.storyTrack == triggerSourceIndex
    then update action model
    else (model, Effects.none)

---- ACTION COMPONENT FUNCTIONS ----
---- These functions are meant to be easily composable to simplify code

removeRepeatBlocks : Model -> (Model, Effects Action) -> (Model, Effects Action)
removeRepeatBlocks oldModel (newModel, effects) =
    if SL.selectedIndex newModel.storyTrack == SL.selectedIndex oldModel.storyTrack
    then (oldModel, Effects.none)
    else (newModel, effects)

moveTrackForward : (Model, Effects Action) -> (Model, Effects Action)
moveTrackForward (model, effects) =
    case model.storyTrack.selected of
        ContentBlock block ->
            let newTrack = SL.next model.storyTrack
                newHistory = model.blockHistory ++ [ model.storyTrack.selected ]
            in ({ model | storyTrack <- newTrack, blockHistory <- newHistory }, effects)

        _ -> (model, effects)

applyTriggers : (Model, Effects Action) -> (Model, Effects Action)
applyTriggers (model, effects) =
    let triggers =
        partitionTriggers model.storyTrack.selected
        |> fst
        |> List.map (\f -> f <| SL.selectedIndex model.storyTrack)
    in (model, Effects.batch <| effects :: triggers)

jumpTo : (Model, Effects Action) -> (String -> Int -> (Model, Effects Action))
jumpTo (model, effects) =
    \label fromChoiceId ->
        case getIndexOfLabel label model of
            Just index ->
                let newTrack   = SL.goto index model.storyTrack
                    newHistory =
                        case model.storyTrack.selected of
                            ChoiceBlock block ->
                                model.blockHistory ++ [ ChoiceBlock { block | selection <- fromChoiceId } ]

                            ContentBlock  _ ->
                                model.blockHistory ++ [ model.storyTrack.selected ]

                            _ -> model.blockHistory

                in ({ model | storyTrack <- newTrack, blockHistory <- newHistory }, effects)

            Nothing -> (model, effects)

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

---- INTERACTIVE STORY VIEW ----

render : Signal.Address Action -> Model -> Html
render address model =
    div
        [ class "interactive-story-container" ]
        (List.map (SB.render address False) model.blockHistory ++ [SB.render address True model.storyTrack.selected])
