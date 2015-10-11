module StoryContent where

import InteractiveStory.StoryBlock exposing (..)
--import Either exposing (..)
--import InteractiveStory.Trigger exposing (autoProgressAfter, performActionAfter, howl)
import InteractiveStory.Action exposing (..)
import AnimationWrapper as AW
import Debug
import Html
import InteractiveStory.Sound as Sound

fadeIn = Sound.fade 0 1 3000
fadeOut = Sound.reverseTransition fadeIn

stuff =
  [ contentBlock "Start - {{ding}}"
    |> (\b -> 
        { b |
            onEnter <- (always { emptyEffectSet | variableEdits <- [UpdateString "ding" <| \str -> Just (Maybe.withDefault "" str)], soundUpdates <- [Sound.bgm "sound1" (Just fadeIn) (Just fadeOut)] }),
            onLeave <- (always { emptyEffectSet | variableEdits <- [SetString "ding" "ding set second"] }),
            label <- Just "start"
        }
    )
  , contentBlock "Block 2 - {{ding}}"
      |> (\b -> 
            { b |
                onEnter <- (always { emptyEffectSet | variableEdits <- [SetString "ding" "ding set third"], soundUpdates <- [Sound.bgm "sound2" (Just fadeIn) (Just fadeOut)] }),
                onLeave <- (always { emptyEffectSet | variableEdits <- [SetString "ding" "ding set fourth"] })
            }
        )
  , contentBlock "This is a string... {{ding}}" |> \b -> { b | label <- Just "goodbye" }
  , choiceBlock "Bleh - {{ding}}" [("Go to hello", Just "hello", Nothing), ("Go to goodbye", Just "goodbye", Nothing), ("Go to start", Just "start", Just (always { emptyEffectSet | variableEdits <- [SetString "ding" "bAck to 1!"] }))] True
  , { emptyStoryBlock | contentGenerator <- \_ _ _ -> Html.text "hello!", label <- Just "hello" }
  ]
