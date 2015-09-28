module InteractiveStory where

import StartApp
import InteractiveStory.Core exposing (..)
import InteractiveStory.Action exposing (..)
import Effects
import Signal exposing ((<~))
import Mouse
import Task exposing (Task)
import Effects exposing (Never)
import Maybe
import Window

import StoryContent


app = StartApp.start {
    init = init StoryContent.stuff,
    view = render,
    update = update,
    inputs = [(always <| Batch [NextBlock]) <~ Mouse.clicks, WindowResize <~ windowDimensions]
    }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks

windowDimensions =
  Signal.merge
    (Signal.sampleOn startAppMailbox.signal Window.dimensions)
    Window.dimensions

startAppMailbox =
  Signal.mailbox ()

port startApp : Signal (Task error ())
port startApp =
  Signal.constant (Signal.send startAppMailbox.address ())
