module InteractiveStory.Sound where

import Time exposing (Time)
import Howler exposing (emptySoundInstance)
import Task exposing (Task, andThen)
import Effects exposing (Effects)

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

type alias LoopModel =
    { sounds : List { priority : Int, sound : Howler.SoundInstance }
    , count  : Int
    }

type alias SoundModel =
    { bgm : LoopModel }

emptySoundModel : SoundModel
emptySoundModel = { bgm = { sounds = [], count = 0 } }

type LoopType = BGM

type SoundAction
    = PlayLoop String LoopType (Maybe Transition) (Maybe Transition)
    | StopLoop LoopType (Maybe Transition)
    | HasStartedLoop LoopType { priority : Int, sound : Howler.SoundInstance }
    | HasStoppedLoop LoopType { priority : Int, sound : Howler.SoundInstance }
    | NoOp

bgm label fadeIn fadeOut = PlayLoop label BGM fadeIn fadeOut 

update : SoundAction -> SoundModel -> (SoundModel, Effects SoundAction)
update action model =
    case action of
        PlayLoop label loopType fadeIn fadeOut ->
            let
                loopModel = case loopType of
                    BGM -> model.bgm

                newCount = loopModel.count + 1

                newEffects =
                    startBGM fadeIn loopModel.count loopType { emptySoundInstance | soundLabel <- label }
                        :: List.map (\{priority, sound} -> stopBGM fadeOut priority loopType sound) loopModel.sounds
                    |> Effects.batch

                newModel = 
                    case loopType of
                        BGM -> { model | bgm <- { loopModel | count <- newCount } }

            in (newModel, newEffects)

        StopLoop loopType fadeOut ->
            let
                loopModel = case loopType of
                    BGM -> model.bgm

                newEffects = 
                    List.map (\{priority, sound} -> stopBGM fadeOut priority loopType sound) loopModel.sounds
                    |> Effects.batch
            in (model, newEffects)

        HasStartedLoop loopType loopObj ->
            let
                loopModel = case loopType of
                    BGM -> model.bgm

                sortedSounds = 
                    loopObj :: loopModel.sounds 
                    |> List.sortBy .priority
                    |> List.reverse

                newEffects = Effects.none
                    --List.drop 1 sortedSounds
                    -- |> List.map (\{priority, sound} -> stopBGM Nothing priority loopType sound)
                    -- |> Effects.batch

                newLoopModel = { loopModel | sounds <- sortedSounds }

                newModel =
                    case loopType of
                        BGM -> { model | bgm <- newLoopModel }

            in (newModel, newEffects)

        HasStoppedLoop loopType loopObj ->
            let
                loopModel = case loopType of
                    BGM -> model.bgm

                newLoopModel = 
                    { loopModel | 
                        sounds <- 
                            loopModel.sounds 
                            |> List.filter ((/=) loopObj)
                    }

                newModel =
                    case loopType of
                        BGM -> { model | bgm <- newLoopModel }
            in (newModel, Effects.none)

        _ -> (model, Effects.none)




emptyTransition : Transition
emptyTransition = { from = 1, to = 1, duration = 0 }

fade : Float -> Float -> Time -> Transition
fade from to duration = { from = from, to = to, duration = duration }

reverseTransition : Transition -> Transition
reverseTransition { from, to, duration } = { from = to, to = from, duration = duration }

tupleToLoopModel (priority, sound) = { priority = priority, sound = sound }

startBGM : Maybe Transition -> Int -> LoopType -> Howler.SoundInstance -> Effects SoundAction
startBGM maybeTransition priority loopType soundInstance =
    (case maybeTransition of
        Nothing ->
            Howler.playSound soundInstance `andThen` Howler.loop True


        Just transition ->
            Howler.playSound soundInstance
            `andThen` Howler.loop True
            `andThen` Howler.fade transition.from transition.to transition.duration
    )
    |> Task.map ((,) priority >> tupleToLoopModel >> HasStartedLoop loopType)
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault NoOp)
    |> Effects.task

stopBGM : Maybe Transition -> Int -> LoopType -> Howler.SoundInstance -> Effects SoundAction
stopBGM maybeTransition priority loopType soundInstance =
    (case maybeTransition of
        Nothing -> Howler.stop soundInstance
        Just transition ->
            Howler.fade transition.from transition.to transition.duration soundInstance
            `andThen` \_ -> Task.sleep transition.duration
            `andThen` \_ -> Howler.stop soundInstance
    )
    |> Task.map ((,) priority >> tupleToLoopModel >> HasStoppedLoop loopType)
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault NoOp)
    |> Effects.task