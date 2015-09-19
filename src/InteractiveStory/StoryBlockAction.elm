module InteractiveStory.StoryBlockAction where

import Time exposing (Time)

type Action
    = AnimateIn
    | Tick Time
    | ChoiceSelect (Maybe Int)
