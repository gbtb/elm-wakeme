module Maps.Internal.Drag exposing
    ( Drag
    , EventOptions
    , drag
    , events
    , offset
    , start
    )

import Html
import Html.Events exposing (on, onMouseUp, preventDefaultOn)
import Json.Decode as Json
import Maps.Internal.Screen as Screen


type Drag
    = StartDrag Screen.Offset
    | Drag Screen.Offset Screen.Offset


type alias EventOptions msg =
    { dragStart : Screen.Offset -> msg
    , dragTo : Screen.Offset -> msg
    , dragStop : msg
    }


start : Screen.Offset -> Drag
start =
    StartDrag


drag : Screen.Offset -> Drag -> Drag
drag offsetValue state =
    case state of
        StartDrag startValue ->
            Drag startValue offsetValue

        Drag _ end ->
            Drag end offsetValue


offset : Drag -> Screen.Offset
offset dragValue =
    case dragValue of
        StartDrag _ ->
            { x = 0, y = 0 }

        Drag startValue end ->
            { x = end.x - startValue.x, y = end.y - startValue.y }


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


events : EventOptions msg -> Maybe Drag -> List (Html.Attribute msg)
events { dragStart, dragTo, dragStop } dragValue =
    [ -- Mouse
      if dragValue == Nothing then
        preventDefaultOn "mousedown" <|
            Json.map alwaysPreventDefault <|
                Json.map dragStart <|
                    Screen.decodeOffset

      else
        on "mousemove" <|
            Json.map dragTo <|
                Screen.decodeOffset
    , -- Mouse
      onMouseUp dragStop

    {- , -- Mobile
         if dragValue == Nothing then
           preventDefaultOn "touchstart" <|
               Json.map alwaysPreventDefault <|
                   Json.map dragStart <|
                       Screen.decodeOffset
         else
           preventDefaultOn "touchmove" <|
               Json.map alwaysPreventDefault <|
                   Json.map dragTo <|
                       Screen.decodeOffset
       , -- Mobile
         preventDefaultOn "touchend" <|
           Json.map alwaysPreventDefault <|
               Json.succeed dragStop
    -}
    ]
