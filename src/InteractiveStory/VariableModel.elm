module InteractiveStory.VariableModel where

import Dict exposing (Dict)

type alias VariableModel = {
    string : Dict String String,
    num    : Dict String Float,
    bool   : Dict String Bool
}

getString : String -> String -> VariableModel -> String
getString key default vars = Dict.get key vars.string |> Maybe.withDefault default

getNum : String -> Float -> VariableModel -> Float
getNum key default vars = Dict.get key vars.num |> Maybe.withDefault default

getBool : String -> Bool -> VariableModel -> Bool
getBool key default vars = Dict.get key vars.bool |> Maybe.withDefault default
