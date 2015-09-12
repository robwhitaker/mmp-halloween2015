module InteractiveStory where

import StartApp
import InteractiveStory.Core exposing (..)
import InteractiveStory.Action exposing (..)
import Effects
import Signal exposing ((<~))
import Mouse
import Task
import Effects exposing (Never)

app = StartApp.start {
    init = (initialModel, Effects.none),
    view = render,
    update = update,
    inputs = [(always NextBlock) <~ Mouse.clicks]
    }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks
