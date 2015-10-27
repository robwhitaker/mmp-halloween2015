module InteractiveStory.Styles.Core where

import Css exposing (Styles)
import Css.Position as Position
import Css.Display as Display
import Css.Margin as Margin
import Css.Dimension as Dimension
import Css.Background as Background
import Color exposing (rgba)
import Css.Padding as Padding
import Css.Border as Border
import Css.Border.Style as BorderStyle
import Css.Margin as Margin
import Css.Gradient as Gradient
import Css.Text as Text
import Css.Font as Font
import Css.ListStyle as ListStyle
import Css.Cursor as Cursor
import Animation exposing (Animation, animation, from, to, duration, delay, animate)
import Html.Events.Extra exposing (ScrollEvent)
import Time exposing (Time)

fullscreen : Styles -> Styles
fullscreen =
    Position.position Position.Absolute
    >> Position.top 0
    >> Position.left 0
    >> Position.right 0
    >> Position.bottom 0
    >> Position.overflow Position.AutoOverflow

bodyDiv styles =
    styles
    |> Gradient.linear 90 [ rgba 255 94 0 1, rgba 91 34 1 1, rgba 0 0 0 1]

topBar : ScrollEvent -> Int -> Styles -> Styles
topBar { scrollTop, scrollHeight, clientHeight } windowWidth styles =
    styles
    |> Dimension.height ( animate (toFloat scrollTop) topBarAnimation )
    |> Dimension.width (windowWidth-17) -- 17 is the width of the scroll bar
    |> Background.color (rgba 0 0 0 0)
    |> Position.zIndex 10

fixed : Styles -> Styles
fixed styles =
    styles
    |> Position.position Position.Fixed
    |> Position.top 0

storyBlock : Styles -> Styles
storyBlock styles =
    styles
    |> Css.style "margin" "15px auto"
    |> Text.color (rgba 200 200 200 1)
    |> Font.size 22
    |> Dimension.maxWidth 600
    |> Background.color (rgba 0 0 0 0.3)
    |> Padding.all 5 15 5 15
    |> Border.color (rgba 0 0 0 1)
    |> Border.width 1 1 1 1
    |> Border.style BorderStyle.Solid
    |> Border.radius 5 5 5 5

animateIn : Time -> Styles -> Styles
animateIn time styles =
    styles
    |> Position.position Position.Relative
    -- |> Position.top (-20 + 20 * (animate time storyBlockAnimation))
    |> Css.style "opacity" (toString <| animate time storyBlockAnimation)

spacer : Int -> Styles -> Styles
spacer windowHeight styles =
    styles
    |> Dimension.height windowHeight

choiceBlockChoice : Bool -> Bool -> Styles -> Styles
choiceBlockChoice selected isActive styles =
    let bgColor =
        if isActive then
            if selected then (Background.color (rgba 144 53 0 1) >> ListStyle.image "assets/images/pumpkin-bullet.png")
            else (Background.color (rgba 0 0 0 0) >> ListStyle.image "assets/images/empty-bullet.png")
        else
            if selected then (Background.color (rgba 144 53 0 0.4) >> ListStyle.image "assets/images/empty-bullet.png")
            else (Background.color (rgba 0 0 0 0) >> ListStyle.image "assets/images/empty-bullet.png")
    in
        styles
        |> ListStyle.image "assets/images/empty-bullet.png"
        |> Border.radius 2 2 2 2
        |> Padding.all 0.5 30 1 30
        |> Cursor.cursor (if isActive then Cursor.Pointer else Cursor.Default)
        |> bgColor

---- ANIMATIONS ----

topBarAnimationFrom = 200

topBarAnimation : Animation
topBarAnimation = animation 0 |> from topBarAnimationFrom |> to 50 |> duration 100 |> delay 50

storyBlockAnimation : Animation
storyBlockAnimation = animation 0 |> from 0 |> to 1 |> duration (0.5 * Time.second)
