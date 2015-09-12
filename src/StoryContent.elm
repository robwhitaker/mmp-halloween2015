module StoryContent where

import InteractiveStory.StoryBlock exposing (..)
import Either exposing (..)
import InteractiveStory.Trigger exposing (autoProgressAfter)

newBlock = { content = Either.Left "", classList = [], triggers = [], stallProgression = 0, label = Nothing }
cBlock = { queryText = "",
          choices   = [],
          classList = [],
          triggers  = [],
          label     = Nothing,
          selection = 0
        }

first = ContentBlock { newBlock | content <- Left "Hi there, this is slide 1!", label <- Just "first" }

stuff = [
    ContentBlock { newBlock | content <- Left "Hi there slide 1, I'm slide 2!", triggers <- [Left <| autoProgressAfter 1000] },
    ContentBlock { newBlock | content <- Left "Slide THREE barging IN!" },
    ChoiceBlock { cBlock | queryText <- "Uhh... I'm 4. People scare me so please talk to someone else...",
                           choices <- [{ queryText = "Talk to 5", jumpToLabel = "five" }, { queryText = "Talk to 1", jumpToLabel = "first" }]
                },
    ContentBlock { newBlock | content <- Left "Ugh, *yawn* what time is it? Too _early_. I'm 5, k? Night.", label <- Just "five" }
    ]

