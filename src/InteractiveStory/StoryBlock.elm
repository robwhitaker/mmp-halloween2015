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
import InteractiveStory.VariableModel exposing (VariableModel)
import InteractiveStory.StoryBlockAction as SBAction
import InteractiveStory.Sound as Sound

import InteractiveStory.Styles.Core exposing (storyBlock, animateIn, storyBlockAnimation, choiceBlockChoice)

import Animation
import AnimationWrapper as AW

import Maybe exposing (andThen)

--import InteractiveStory.EffectSet exposing (..)
import Regex
import String
import Dict
import Debug

---- STORYBLOCK MODEL ----

type alias Choice =
    { text  : VariableModel -> Html
    , jumpToLabel : Maybe String
    , onChoose : VariableModel -> EffectSet
    , chosen : Bool
    }

emptyChoice : Choice
emptyChoice = { text = \_ -> Html.text "", jumpToLabel = Nothing, onChoose = \_ -> emptyEffectSet, chosen = False }

type alias ChoiceModel =
    { selection : Maybe Int
    , choices : List Choice
    , showChosen : Bool
    }

emptyChoiceModel : ChoiceModel
emptyChoiceModel = { selection = Nothing, choices = [], showChosen = True }

type alias StoryBlock =
    { contentGenerator : Bool -> VariableModel -> Signal.Address Action -> Html
    , onEnter : VariableModel -> EffectSet
    , onLeave : VariableModel -> EffectSet
    , next : VariableModel -> Next
    , label : Maybe String
    , choiceModel : Maybe ChoiceModel
    , animationState : AW.AnimationWrapper
    }

emptyStoryBlock : StoryBlock
emptyStoryBlock =
    { contentGenerator = \_ _ _ -> Html.text ""
    , onEnter = \_ -> emptyEffectSet
    , onLeave = \_ -> emptyEffectSet
    , next = \_ -> Continue
    , label = Nothing
    , choiceModel = Nothing
    , animationState = AW.empty
    }

---- HELPERS ----

injectVariables : VariableModel -> String -> String
injectVariables vars str =
    let regex = Regex.regex "\\{\\{.*?\\}\\}" |> Regex.caseInsensitive
        replaceFn { match } =
            match
            |> String.dropLeft 2
            |> String.dropRight 2
            |> (\str ->
                Maybe.oneOf
                    (Dict.get str vars.string ::
                        (Maybe.map toString << Dict.get str) vars.num ::
                            (Maybe.map toString << Dict.get str) vars.bool ::
                                []
                    )
                )
            |> Maybe.withDefault match
        injectVars = Regex.replace Regex.All regex replaceFn
    in injectVars str

animationInProgress : StoryBlock -> Bool
animationInProgress = .animationState >> AW.isDone >> not

---- PRESETS ----

contentBlock : String -> StoryBlock
contentBlock str = { emptyStoryBlock | contentGenerator <- \_ vars _ -> Markdown.toHtml <| injectVariables vars str }

choiceBlock : String -> List (String, Maybe String) -> Bool -> StoryBlock
choiceBlock str choices showChosen =
    { emptyStoryBlock |
        contentGenerator <- (\_ vars _ -> Markdown.toHtml <| injectVariables vars str),
        next <- (\_ -> Stop),
        choiceModel <- Just
            { emptyChoiceModel |
                choices <-
                    List.map (\(txt, label) ->
                        { emptyChoice |
                            text <- (\vars -> Markdown.toHtml <| injectVariables vars txt),
                            jumpToLabel <- label
                        }) choices,
                showChosen <- showChosen
            }
    }

---- STORYBLOCK UPDATE ----

update : SBAction.Action -> StoryBlock -> (StoryBlock, Effects SBAction.Action)
update action storyBlock =
    case action of
        SBAction.Init ->
            ({ storyBlock |
                choiceModel <-
                    storyBlock.choiceModel
                    |> Maybe.map (\choiceModel ->
                        { choiceModel |
                            choices <-
                                if choiceModel.showChosen
                                then choiceModel.choices
                                else List.filter (not << .chosen) choiceModel.choices,
                            selection <- Nothing
                        }
                    )
            }, Effects.none)

        SBAction.ChoiceSelect newSelection ->
            case storyBlock.choiceModel of
                Just choiceModel ->
                    ({ storyBlock | choiceModel <- Just { choiceModel | selection <- newSelection }}, Effects.none)
                Nothing -> (storyBlock, Effects.none)

        SBAction.ChoiceConfirm ->
            let newChoiceModel =
                storyBlock.choiceModel
                `andThen` \choiceModel -> choiceModel.selection
                `andThen` \selection ->
                    let newChoices =
                        List.indexedMap
                            (\index choice -> { choice | chosen <- index == selection || choice.chosen })
                            choiceModel.choices
                    in Just { choiceModel | choices <- newChoices }
            in ({ storyBlock | choiceModel <- newChoiceModel }, Effects.none)

        SBAction.AnimateIn ->
            let (newAnimationState, newEffects) = AW.update (AW.Start storyBlockAnimation) storyBlock.animationState
                newEffects' = Effects.map (SBAction.Tick) newEffects
            in ({ storyBlock | animationState <- newAnimationState }, newEffects')

        SBAction.Tick awaction ->
            let (newAnimationState, newEffects) = AW.update awaction storyBlock.animationState
                newEffects' = Effects.map (SBAction.Tick) newEffects
            in ({ storyBlock | animationState <- newAnimationState }, newEffects')

        _ -> (storyBlock, Effects.none)

---- STORYBLOCK RENDER ----

render : Signal.Address Action -> Bool -> VariableModel -> StoryBlock -> Html
render address isActive vars block =
    let animationTime = AW.query always block.animationState
    in
        div
            [ class "story-block", style <| animateIn animationTime <| storyBlock [] ]
            [ block.contentGenerator isActive vars address
            , block.choiceModel
                `andThen` (\choiceModel ->
                    Just (choicesToHtmlList address isActive choiceModel.selection choiceModel.showChosen vars choiceModel.choices)
                )
              |> Maybe.withDefault (Html.text "")
            ]

choicesToHtmlList : Signal.Address Action -> Bool -> Maybe Int -> Bool -> VariableModel -> List Choice -> Html
choicesToHtmlList address isActive selection showChosen vars choices =
    let choiceToLi index { text, jumpToLabel, onChoose, chosen } =
        if isActive
        then
            let actionList =
                [ StoryBlockAction SBAction.ChoiceConfirm
                , RunEffectSet (onChoose vars)
                , Maybe.map JumpToLabel jumpToLabel |> Maybe.withDefault NoOp
                ]
            in li
                [ onClick address <| Batch actionList
                , onMouseEnter address <| StoryBlockAction (SBAction.ChoiceSelect (Just index))
                , onMouseLeave address <| StoryBlockAction (SBAction.ChoiceSelect Nothing)
                , style <| choiceBlockChoice (Just index == selection) isActive [] ] [ text vars ]
        else li
            [ onClick address NoOp
            , onMouseEnter address NoOp
            , onMouseLeave address NoOp
            , style <| choiceBlockChoice (Just index == selection) isActive [] ] [ text vars ]
    in ul [] <| List.indexedMap choiceToLi choices
