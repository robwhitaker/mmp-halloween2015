module InteractiveStory.Sound where

import Time exposing (Time)
import Howler exposing (emptySoundInstance)
import Task exposing (Task, andThen)
import Either exposing (..)

import Debug

type BGMAction
    = Play String (Maybe Transition) (Maybe Transition)
    | Pause
    | Stop (Maybe Transition)

type alias SFX =
    { label  : String
    , sprite : Maybe String
    , delay  : Time
    }

type alias Transition =
    { from : Float
    , to : Float
    , duration : Time
    }

emptyTransition : Transition
emptyTransition = { from = 1, to = 1, duration = 0 }

fade : Float -> Float -> Time -> Transition
fade from to duration = { from = from, to = to, duration = duration }

reverseTransition : Transition -> Transition
reverseTransition { from, to, duration } = { from = to, to = from, duration = duration }

runBGMAction : BGMAction -> Maybe Howler.SoundInstance -> Task x Howler.SoundInstance
runBGMAction bgm currentSound =
    case bgm of
        Play label fadeIn fadeOut ->
            case currentSound of
                Nothing -> startBGM fadeIn { emptySoundInstance | soundLabel <- label }
                Just sound ->
                    startBGM fadeIn { emptySoundInstance | soundLabel <- label }
                    `andThen` \newBGM -> stopBGM fadeOut sound
                    `andThen` \_ -> Task.succeed newBGM

        Pause ->
            case currentSound of
                Nothing -> Howler.pause emptySoundInstance
                Just sound -> Howler.pause sound
        Stop fadeOut ->
            case currentSound of
                Nothing -> Howler.stop emptySoundInstance
                Just sound -> stopBGM fadeOut sound

startBGM : Maybe Transition -> Howler.SoundInstance -> Task x Howler.SoundInstance
startBGM maybeTransition soundInstance =
    case maybeTransition of
        Nothing ->
            Howler.playSound soundInstance
            `andThen` Howler.loop True

        Just transition ->
            Howler.playSound soundInstance
            `andThen` Howler.loop True
            `andThen` Howler.fade transition.from transition.to transition.duration

stopBGM : Maybe Transition -> Howler.SoundInstance -> Task x Howler.SoundInstance
stopBGM maybeTransition soundInstance =
    case maybeTransition of
        Nothing -> Howler.stop soundInstance
        Just transition ->
            Howler.fade transition.from transition.to transition.duration soundInstance
            `andThen` \_ -> Task.sleep transition.duration
            `andThen` \_ -> Howler.stop soundInstance



playSFX : SFX -> Task x Howler.SoundInstance
playSFX { label, sprite, delay } =
    Task.sleep delay
    `andThen` \_ -> Howler.play sprite { emptySoundInstance | soundLabel <- label }
    `andThen` Howler.loop False


playSFXList : List SFX -> Task x (List Howler.SoundInstance)
playSFXList = List.map playSFX >> Task.sequence
