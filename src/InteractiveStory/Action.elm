module InteractiveStory.Action where

---- ACTION MODEL ----

type Action
    = NextBlock
    | JumpToLabel String Int -- label, fromChoiceId
    | Trigger Action Int -- current index in story track
    | NoOp
