module Maps.Internal.Drag exposing
    ( Drag
    , EventOptions
    , drag
    , events
    , offset
    , start
    )

import Html
import Html.Events exposing (custom, on, onMouseUp, preventDefaultOn)
import Json.Decode as Json
import Maps.Internal.Screen as Screen
import Maps.Internal.Utils exposing (noDefaultButPropagate)


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
    ( msg, False )


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
    , -- Mobile
      {- custom "touchstart" <|
             Json.map noDefaultButPropagate <|
                 Json.map (Maybe.withDefault pinchStop) <|
                     Json.map (Maybe.map pinchStart) <|
                         Screen.decodeTwoFingers
         , -- Mobile
           custom "touchmove" <|
             Json.map noDefaultButPropagate <|
                 Json.map (Maybe.withDefault pinchStop) <|
                     Json.map (Maybe.map pinchTo) <|
                         Screen.decodeTwoFingers
         , -- Mobile
           custom "touchend" <|
             Json.map noDefaultButPropagate <|
                 Json.succeed pinchStop
      -}
      if dragValue == Nothing then
        custom "touchstart" <|
            Json.map noDefaultButPropagate <|
                Json.map dragStart <|
                    Screen.decodeOffset

      else
        custom "touchmove" <|
            Json.map noDefaultButPropagate <|
                Json.map dragTo <|
                    Screen.decodeOffset
    , -- Mobile
      custom "touchend" <|
        Json.map noDefaultButPropagate <|
            Json.succeed dragStop
    ]
