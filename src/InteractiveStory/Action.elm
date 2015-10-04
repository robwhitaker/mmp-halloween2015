module InteractiveStory.Action where

import InteractiveStory.StoryBlockAction as SBAction
import Html.Events.Extra exposing (ScrollEvent)
import Animation exposing (Animation)
import Time exposing (Time)
import Howler
import AnimationWrapper as AW
import InteractiveStory.Sound as Sound
--import InteractiveStory.EffectSet exposing (..)

---- ACTION MODEL ----

type Action
    = NextBlock
    | JumpToLabel String
    -- | Trigger Action (Maybe Int) -- current index in story track
    | EditVar VarAction
    | Batch (List Action)
    | StoryBlockAction SBAction.Action
    | WindowResize (Int, Int)
    | UserScroll ScrollEvent
    | AnimateScroll Animation
    | ApplyChunking Float
    | ScrollTick AW.Action
    | RunEffectSet EffectSet
    -- | UpdateBGM (Maybe Howler.SoundInstance)
    | NoOp

type VarAction
    = SetString String String
    | SetNum String Float
    | SetBool String Bool
    | UpdateString String (Maybe String -> Maybe String)
    | UpdateNum String (Maybe Float -> Maybe Float)
    | UpdateBool String (Maybe Bool -> Maybe Bool)

type alias EffectSet =
    { variableEdits : List VarAction
    --, bgm : Maybe Sound.BGMAction
    --, sfx : List Sound.SFX
    --, triggers : List Action
    }

emptyEffectSet : EffectSet
emptyEffectSet = { variableEdits = [] }

type Next
    = Continue
    | Label String
    | Stop
