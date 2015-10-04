module Howler where

import Native.Howler
import Task exposing (Task)
import Dict exposing (Dict)
import Time exposing (Time)

import Debug

type alias AudioObject = {
    src            : List String,
    loop           : Maybe Bool,
    volume         : Maybe Float,
    html5          : Maybe Bool,
    sprite         : Maybe (Dict String (Float, Float, Bool)),
    rate           : Maybe Float,
    pool           : Maybe Int
}

emptyAudioObject : AudioObject
emptyAudioObject =
    { src = []
    , loop = Nothing
    , volume = Nothing
    , html5 = Nothing
    , sprite = Nothing
    , rate = Nothing
    , pool = Nothing
    }

type alias PlayId = Int
type alias SoundLabel = String
type alias SpriteLabel = String

type alias SoundInstance = { soundLabel : SoundLabel, playId : Maybe PlayId }

emptySoundInstance : SoundInstance
emptySoundInstance = { soundLabel = "", playId = Nothing }

---- CONTROLS ----

create : SoundLabel -> AudioObject -> Task x SoundInstance
create = Native.Howler.create

play : Maybe SpriteLabel -> SoundInstance -> Task x SoundInstance
play = Native.Howler.play

playSound : SoundInstance -> Task x SoundInstance
playSound = play Nothing

playSprite : SpriteLabel -> SoundInstance -> Task x SoundInstance
playSprite = Just >> play

pause : SoundInstance -> Task x SoundInstance
pause = Native.Howler.pause

stop : SoundInstance -> Task x SoundInstance
stop = Native.Howler.stop

mute : Bool -> SoundInstance -> Task x SoundInstance
mute = Native.Howler.mute

volume : Float -> SoundInstance -> Task x SoundInstance
volume = Native.Howler.volume

fade : Float -> Float -> Time -> SoundInstance -> Task x SoundInstance
fade = Native.Howler.fade

seek : Float -> SoundInstance -> Task x SoundInstance
seek = Native.Howler.seek

loop : Bool -> SoundInstance -> Task x SoundInstance
loop = Native.Howler.loop

---- QUERIES ----

isPlaying : SoundInstance -> Task x Bool
isPlaying = Native.Howler.isPlaying

getDuration : SoundInstance -> Task x Float
getDuration = Native.Howler.getDuration

isMuted : SoundInstance -> Task x Bool
isMuted = Native.Howler.isMuted

getVolume : SoundInstance -> Task x Float
getVolume = Native.Howler.getVolume

getSeek : SoundInstance -> Task x Float
getSeek = Native.Howler.getSeek

isLooping : SoundInstance -> Task x Bool
isLooping = Native.Howler.isLooping

------ CALLBACK HANDLING ----

-- DISABLED BECAUSE HOWLER INSISTS ON FIRING EVENTS IMMEDIATELY INSTEAD OF AT THE CORRECT TIMES

--on : String -> Signal.Address action -> action -> SoundObject -> Task x SoundObject
--on event address action soundObject =
--    Native.Howler.on
--        event
--        (\_ -> Signal.send address action)
--        soundObject

--once : String -> ((PlayObject, String) -> action) -> Event action
--once event wrapperFunction = { event = event, oneOff = True, wrapperFunction = wrapperFunction }
