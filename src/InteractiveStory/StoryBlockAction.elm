module InteractiveStory.StoryBlockAction where

import Time exposing (Time)
import AnimationWrapper as AW
type Action
    = AnimateIn
    | Tick AW.Action
    | ChoiceSelect (Maybe Int)
