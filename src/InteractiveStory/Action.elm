module InteractiveStory.Action where

import InteractiveStory.StoryBlockAction as SBAction
import Html.Events.Extra exposing (ScrollEvent)
import Animation exposing (Animation)
import Time exposing (Time)

---- ACTION MODEL ----

type Action
    = NextBlock
    | JumpToLabel String
    | Trigger Action (Maybe Int) -- current index in story track
    | EnableProgression
    | EditVar VarAction Bool -- trigger live var update
    | Batch (List Action)
    | StoryBlockAction SBAction.Action
    | WindowResize (Int, Int)
    | UserScroll ScrollEvent
    | AnimateScroll Animation
    | ApplyChunking Float
    | Tick Time
    | NoOp

type VarAction
    = SetString String String
    | SetNum String Float
    | SetBool String Bool
    | UpdateString String (Maybe String -> Maybe String)
    | UpdateNum String (Maybe Float -> Maybe Float)
    | UpdateBool String (Maybe Bool -> Maybe Bool)
