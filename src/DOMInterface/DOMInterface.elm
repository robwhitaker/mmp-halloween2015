module DOMInterface where

import Native.DOMInterface
import Task exposing (Task)

getElementPositionInfo : String -> Task Error (List PositionInfo)
getElementPositionInfo =
    Native.DOMInterface.getElementPositionInfo

scrollElementTo : (Int, Int) -> String -> Task Error ()
scrollElementTo =
    Native.DOMInterface.scrollElementTo

type alias PositionInfo = {
    top    : Float,
    left   : Float,
    width  : Float,
    height : Float,
    client : { width : Float, height : Float },
    offset : { width : Float, height : Float },
    margin : { top : Float, right : Float, bottom : Float, left : Float }
}

type Error
    = NodeUndefined
