module Html.Events.Extra where

import Json.Decode exposing (..)
import Html.Events exposing (on)
import Html exposing (Attribute)
import Signal exposing (Address)

type alias ScrollEvent = {
    scrollTop    : Int,
    scrollLeft   : Int,
    scrollHeight : Int,
    scrollWidth  : Int,
    clientHeight : Int,
    clientWidth  : Int
}

scrollDecoder : Decoder ScrollEvent
scrollDecoder =
    let
        targetDecoder =
            object6 ScrollEvent
                ("scrollTop" := int)
                ("scrollLeft" := int)
                ("scrollHeight" := int)
                ("scrollWidth" := int)
                ("clientHeight" := int)
                ("clientWidth" := int)

    in "target" := targetDecoder


onScroll : Address a -> (ScrollEvent -> a) -> Attribute
onScroll address f = on "scroll" scrollDecoder (\scrollEvent -> Signal.message address (f scrollEvent))
