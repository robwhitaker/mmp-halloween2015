module InteractiveStory.VariableModel where

import Dict exposing (Dict)

type alias VariableModel = {
    string : Dict String String,
    num    : Dict String Float,
    bool   : Dict String Bool
}
