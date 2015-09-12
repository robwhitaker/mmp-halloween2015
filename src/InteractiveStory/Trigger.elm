module InteractiveStory.Trigger where

import Time exposing (Time)
import Effects exposing (Effects)
import Task
import InteractiveStory.Action exposing (..)

---- TRIGGER BUILDERS ----

type alias TriggerBuilder = (Int -> Effects Action)

autoProgressAfter : Time -> TriggerBuilder
autoProgressAfter time =
    \index ->
        Task.sleep time
        |> Task.map (always <| Trigger NextBlock index)
        |> Effects.task



