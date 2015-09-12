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
---- STORYBLOCK MODEL ----

type alias Trigger = Either TriggerBuilder String

type StoryBlock
    = ContentBlock
        { content            : Either String Html,
          classList          : List String,
          triggers           : List Trigger,
          stallProgression   : Time,
          label              : Maybe String
        }
    | ChoiceBlock
        { queryText : String,
          choices   : List Choice,
          classList : List String,
          triggers  : List Trigger,
          label     : Maybe String,
          selection : Int
        }
    | EndBlock { label : Maybe String, triggers: List Trigger }

type alias Choice = {
    queryText   : String,
    jumpToLabel : String
}


getLabel : StoryBlock -> Maybe String
getLabel storyBlock =
    case storyBlock of
        ContentBlock { label } -> label
        ChoiceBlock  { label } -> label
        EndBlock     { label } -> label

getTriggers : StoryBlock -> List Trigger
getTriggers storyBlock =
    case storyBlock of
        ContentBlock { triggers } -> triggers
        ChoiceBlock  { triggers } -> triggers
        EndBlock     { triggers } -> triggers

initialStoryBlock : StoryBlock
initialStoryBlock = ContentBlock { content = Either.Left "", classList = [], triggers = [], stallProgression = 0, label = Nothing }



---- STORYBLOCK RENDER ----

render : Signal.Address Action -> Bool -> StoryBlock -> Html
render address isActive block =
    case block of
        ContentBlock { content, classList, triggers, stallProgression, label } ->
            div
                [ Attr.classList <| ("active-block", isActive) :: makeClassList "story-block" classList ]
                [ contentToHtml content ]

        ChoiceBlock { queryText, choices, classList, triggers, label, selection } ->
            div
                [ Attr.classList <| ("active-block", isActive) :: makeClassList "choice-block" classList ]
                [ Markdown.toHtml queryText,
                  choicesToHtmlList address isActive selection choices
                ]

        EndBlock _ -> div [] [ text "End" ]

makeClassList : String -> List String -> List (String, Bool)
makeClassList baseClass additionalClasses =
    (baseClass, True) :: List.map (flip (,) True) additionalClasses

contentToHtml : Either String Html -> Html
contentToHtml content =
    case content of
        Either.Left txt   -> Markdown.toHtml txt
        Either.Right html -> html

choicesToHtmlList : Signal.Address Action -> Bool -> Int -> List Choice -> Html
choicesToHtmlList address isActive selection choices =
    let choiceToLi index { queryText, jumpToLabel } =
        if isActive
        then li [ onClick address <| JumpToLabel jumpToLabel index ] [ text queryText ]
        else li [ class <| if index == selection then "selected-choice" else "" ] [ text queryText ]
    in ul [] <| List.indexedMap choiceToLi choices
