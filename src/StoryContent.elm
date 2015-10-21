module StoryContent where

import InteractiveStory.StoryBlock exposing (..)
--import Either exposing (..)
--import InteractiveStory.Trigger exposing (autoProgressAfter, performActionAfter, howl)
import InteractiveStory.Action exposing (..)
import AnimationWrapper as AW
import Debug
import Html
import InteractiveStory.Sound as Sound
import InteractiveStory.VariableModel as VM

fadeIn = Sound.fade 0 1 3000
fadeOut = Sound.reverseTransition fadeIn

stuff =
  [ contentBlock "Start - {{ding}}"
    |> (\b ->
        { b |
            onEnter <- (always { emptyEffectSet | variableEdits <- [UpdateString "ding" <| \str -> Just (Maybe.withDefault "" str)], soundUpdates <- [Sound.bgm "sound1" (Just fadeIn) (Just fadeOut)] }),
            onLeave <- (always { emptyEffectSet | variableEdits <- [SetString "ding" "ding set second"], soundUpdates <- [Sound.sfx "sound3" (Just "sprite1") 1500] }),
            label <- Just "start"
        }
    )
  , contentBlock "Block 2 - {{ding}}"
      |> (\b ->
            { b |
                onEnter <- (always { emptyEffectSet | variableEdits <- [SetString "ding" "ding set third"] }),
                onLeave <- (always { emptyEffectSet | variableEdits <- [SetString "ding" "ding set fourth"], soundUpdates <- [Sound.StopSound "sound1"] })
            }
        )
  , contentBlock "This is a string... {{ding}}" |> \b -> { b | label <- Just "goodbye" }
  , choiceBlock "Bleh - {{ding}}" [("Go to hello", Just "hello", Nothing, Nothing), ("Go to goodbye", Just "goodbye", Nothing, Nothing), ("Go to start", Just "start", Just (always { emptyEffectSet | variableEdits <- [SetString "ding" "bAck to 1!", SetBool "beenToEnd" True]}), Just (VM.getBool "beenToEnd" False >> not))] True
  , { emptyStoryBlock | contentGenerator <- \_ _ _ -> Html.text "hello!", label <- Just "hello" }
  ]
