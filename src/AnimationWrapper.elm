module AnimationWrapper where

import Animation exposing (Animation)
import Time exposing (Time)
import Effects exposing (Effects)
import Maybe

---- ANIMATION WRAPPER MODEL ----

type alias AnimationWrapper =
    { prevClockTime : Maybe Time
    , elapsedTime   : Time
    , animation     : Animation
    , isStopped     : Bool
    }

empty : AnimationWrapper
empty = { prevClockTime = Nothing, elapsedTime = 0, animation = Animation.static 0, isStopped = True }

---- ANIMATION WRAPPER UPDATE ----

type Action
    = Start Animation
    | Stop
    | Resume
    | Seek Time
    | Tick Time

update : Action -> AnimationWrapper -> (AnimationWrapper, Effects Action)
update action animationWrapper =
    case action of
        Start newAnimation ->
            (,)
                { empty | animation <- newAnimation, isStopped <- False }
                ( Effects.tick Tick )

        Stop ->
            (,)
                { animationWrapper | prevClockTime <- Nothing, isStopped <- True }
                Effects.none

        Resume ->
            (,)
                { animationWrapper | isStopped <- False }
                ( Effects.tick Tick )

        Seek time ->
            (,)
                { animationWrapper | elapsedTime <- time }
                Effects.none

        Tick clockTime ->
            if isDone animationWrapper || isStopped animationWrapper
            then
                (,)
                    { animationWrapper | prevClockTime <- Nothing }
                    ( Effects.none )
            else
                case animationWrapper.prevClockTime of
                    Nothing ->
                        (,)
                            { animationWrapper | prevClockTime <- Just clockTime }
                            ( Effects.tick Tick )

                    Just prevTime ->
                        (,)
                            { animationWrapper
                                | prevClockTime <- Just clockTime
                                , elapsedTime <- animationWrapper.elapsedTime + (clockTime - prevTime)
                            }
                            ( Effects.tick Tick )

---- ANIMATION WRAPPER ACCESSORS ----

value : AnimationWrapper -> Float
value { elapsedTime, animation } = Animation.animate elapsedTime animation

isDone : AnimationWrapper -> Bool
isDone { elapsedTime, animation } = Animation.isDone elapsedTime animation

isStopped : AnimationWrapper -> Bool
isStopped = .isStopped

query : (Float -> Animation -> a) -> AnimationWrapper -> a
query queryFn { elapsedTime, animation } = queryFn elapsedTime animation

