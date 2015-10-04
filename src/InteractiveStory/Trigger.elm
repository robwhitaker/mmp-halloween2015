module InteractiveStory.Trigger where

import Time exposing (Time)
import Effects exposing (Effects, Never)
import Task exposing (andThen)
import InteractiveStory.Action exposing (..)
import Dict

import DOMInterface
import Debug
import Howler exposing (emptyAudioObject)

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

howl : TriggerBuilder
howl =
    \index ->
        --Howler.create "sound1" { empty | src <- ["sound1.mp3"], volume <- Just 0.3, sprite <- Just <| Dict.fromList [("ding", (3000, 2000, True)), ("dong", (10000,500, True))] }
        --`andThen` Howler.playSprite "dong"
        --`andThen` Howler.playSprite "ding"
        --`andThen` (\_ ->
        Howler.create "sound2" { emptyAudioObject | src <- ["sound1.mp3"], loop <- Just True }
        `andThen` Howler.playSound
        `andThen` Howler.seek 10
        `andThen` (\sound ->
            Task.map (Debug.log "playing") (Howler.isPlaying sound)
            `andThen` \_ -> Task.map (Debug.log "duration") (Howler.getDuration sound)
            `andThen` \_ -> Task.map (Debug.log "muted") (Howler.isMuted sound)
            `andThen` \_ -> Task.map (Debug.log "volume") (Howler.getVolume sound)
            `andThen` \_ -> Task.map (Debug.log "seek") (Howler.getSeek sound)
            `andThen` \_ -> Task.map (Debug.log "looping") (Howler.isLooping sound)
        )
        |> Task.toMaybe
        |> Task.map (Debug.log "response")
        |> Task.map (always NoOp)
        |> Effects.task
