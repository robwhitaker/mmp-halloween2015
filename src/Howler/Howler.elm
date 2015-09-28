module Howler where

import Native.Howler
import Task exposing (Task)
import Dict exposing (Dict)
import Time exposing (Time)

type alias AudioObject action = {
    src            : List String,
    loop           : Maybe Bool,
    volume         : Maybe Float,
    html5          : Maybe Bool,
    sprite         : Maybe (Dict String (Float, Float, Bool)),
    rate           : Maybe Float,
    pool           : Maybe Int
}

empty : AudioObject Int
empty =
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

type alias SoundObject = { soundLabel : SoundLabel, playId : Maybe PlayId }
type alias Event action = { event : String, oneOff : Bool, wrapperFunction : (SoundObject, String) -> action }

---- CONTROLS ----

create : SoundLabel -> AudioObject action -> Task x SoundObject
create = Native.Howler.create

play : Maybe SpriteLabel -> SoundObject -> Task x SoundObject
play = Native.Howler.play

playSound : SoundObject -> Task x SoundObject
playSound = play Nothing

playSprite : SpriteLabel -> SoundObject -> Task x SoundObject
playSprite = Just >> play

pause : SoundObject -> Task x SoundObject
pause = Native.Howler.pause

stop : SoundObject -> Task x SoundObject
stop = Native.Howler.stop

mute : Bool -> SoundObject -> Task x SoundObject
mute = Native.Howler.mute

volume : Float -> SoundObject -> Task x SoundObject
volume = Native.Howler.volume

fade : Float -> Float -> Time -> SoundObject -> Task x SoundObject
fade = Native.Howler.fade

seek : Float -> SoundObject -> Task x SoundObject
seek = Native.Howler.seek

loop : Bool -> SoundObject -> Task x SoundObject
loop = Native.Howler.loop

---- QUERIES ----

isPlaying : SoundObject -> Task x Bool
isPlaying = Native.Howler.isPlaying

getDuration : SoundObject -> Task x Float
getDuration = Native.Howler.getDuration

isMuted : SoundObject -> Task x Bool
isMuted = Native.Howler.isMuted

getVolume : SoundObject -> Task x Float
getVolume = Native.Howler.getVolume

getSeek : SoundObject -> Task x Float
getSeek = Native.Howler.getSeek

isLooping : SoundObject -> Task x Bool
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
