module StoryContent where

import InteractiveStory.StoryBlock exposing (..)
import Either exposing (..)
import InteractiveStory.Trigger exposing (autoProgressAfter, performActionAfter)
import InteractiveStory.Action exposing (..)
import AnimationWrapper as AW
import Debug

newBlock = { content = "", variableEdits = [], triggers = [], disableProgression = False, label = Nothing, animationState = AW.empty }
cBlock = { queryText = "",
          choices   = [],
          variableEdits = [],
          triggers  = [],
          label     = Nothing,
          selection = Nothing, animationState = AW.empty,
          selectionLocked = False
        }



stuff = [
    LogicBlock { label = Nothing, run = \_ -> [EditVar (SetString "atOne" "sure am!") True]  },
    ContentBlock { newBlock |
        content <-
          """{{ding}} {{ding}} {{ding}}Hi there,

              this is slide 1 {{okay}}! {{doobly}}.
              This is

                  all {{atOne}}
""",
        label <- Just "first" ,
        triggers <- [Left <| autoProgressAfter 600],
        variableEdits <- [(SetString "doobly" "Big jim bob"), SetString "okay" "dokay"]
      },
    ContentBlock { newBlock | content <- "Hi there slide 1, I'm slide 2! Count {{ding}} and {{doobly}}", triggers <- [Left <| autoProgressAfter 600] },
    ContentBlock { newBlock | content <- "Slide THREE barging IN with a {{ding}}!", triggers <- [Left <| autoProgressAfter 600] },
     ChoiceBlock { cBlock | queryText <- "Uhh... I'm 4. People scare me so please talk to someone else...",
                           choices <- [{ queryText = "Talk to 5 - {{ding}}", jumpToLabel = "five", variableEdits = [(SetString "at-five" "600")], triggerLiveVarUpdate = False },
                                      { queryText = "Talk to 1", jumpToLabel = "first", variableEdits = [SetBool "atOne" True, (UpdateNum "ding" (\n -> Maybe.withDefault -1 n |> (+) 1 |> Just))], triggerLiveVarUpdate = False }]
                },
    ContentBlock { newBlock | content <- "Ugh, *yawn* what time is it? Too _early_. I'm {{at-five}}5, k? Night.", label <- Just "five", triggers <- [Left <| autoProgressAfter 600] },
    LogicBlock { label = Nothing, run = \_ -> [ JumpToLabel "first" ] },
    EndBlock { label = Nothing, triggers = [], animationState = AW.empty }
    ]

