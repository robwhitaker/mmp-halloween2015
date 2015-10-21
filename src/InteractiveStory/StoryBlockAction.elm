module InteractiveStory.StoryBlockAction where

import Time exposing (Time)
import AnimationWrapper as AW
import InteractiveStory.VariableModel exposing (VariableModel)

type Action
    = AnimateIn
    | Tick AW.Action
    | ChoiceSelect (Maybe Int)
    | ChoiceConfirm
    | Init VariableModel
