module DOMInterface
    ( getElementPositionInfo
    , scrollElementTo
    , Error
    , PositionInfo
    ) where

import Native.DOMInterface
import Task exposing (Task)

getElementPositionInfo : String -> Int -> Task Error PositionInfo
getElementPositionInfo =
    Native.DOMInterface.getElementPositionInfo

scrollElementTo : (Int, Int) -> String -> Int -> Task Error ()
scrollElementTo =
    Native.DOMInterface.scrollElementTo

type alias PositionInfo = {
    top        : Float,
    left       : Float,
    width      : Float,
    height     : Float,

    offsetParent : Maybe {
        top        : Float,
        left       : Float,
        width      : Float,
        height     : Float
    },

    directParent : Maybe {
        top    : Float,
        left   : Float,
        width  : Float,
        height : Float
    }
}

type Error
    = NodeUndefined
