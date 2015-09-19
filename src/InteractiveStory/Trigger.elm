module InteractiveStory.Trigger where

import Time exposing (Time)
import Effects exposing (Effects, Never)
import Task exposing (andThen)
import InteractiveStory.Action exposing (..)

import DOMInterface
import Debug

---- TRIGGER BUILDERS ----

type alias TriggerBuilder = (Int -> Effects Action)

performActionAfter : Time -> Action -> TriggerBuilder
performActionAfter time action =
    \index ->
        Task.sleep time
        |> Task.map (always <| Trigger action <| Just index)
        |> Effects.task

autoProgressAfter : Time -> TriggerBuilder
autoProgressAfter = flip performActionAfter NextBlock

domdom : TriggerBuilder
domdom =
    \index ->
        DOMInterface.scrollElementTo (0, 100) ".interactive-story-container" 0
        |> Task.toMaybe
        |> Task.map (always NoOp)
        |> Effects.task
