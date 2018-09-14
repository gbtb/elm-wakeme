module Maps.Internal.Zoom exposing
    ( EventOptions
    , events
    , fromPinch
    )

import Html
import Html.Events exposing (custom, on)
import Json.Decode as Json
import Maps.Internal.Pinch as Pinch exposing (Pinch)
import Maps.Internal.Screen as Screen exposing (ZoomLevel)
import Maps.Internal.Utils exposing (noDefaultButPropagate)


type alias EventOptions msg =
    { zoom : Screen.Offset -> ZoomLevel -> msg
    , pinchStart : Screen.TwoFingers -> msg
    , pinchTo : Screen.TwoFingers -> msg
    , pinchStop : msg
    }


fromPinch : Float -> Float -> Pinch -> ( ZoomLevel, Screen.Offset )
fromPinch mapWidth mapHeight pinch =
    let
        ( start, end ) =
            Pinch.startEnd pinch
    in
    ( logBase 2 (end.length / start.length)
    , start.center
    )


events : EventOptions msg -> ZoomLevel -> List (Html.Attribute msg)
events { zoom, pinchStart, pinchTo, pinchStop } mapZoom =
    [ -- Mouse
      custom "dblclick" <|
        Json.map noDefaultButPropagate <|
            Json.map (\offset -> zoom offset 1) <|
                Screen.decodeOffset
    , --Mouse
      custom "wheel" <|
        Json.map noDefaultButPropagate <|
            Json.map2
                zoom
                Screen.decodeOffset
                Screen.decodeZoom

    {- , -- Mobile
         custom "touchstart" <|
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
    ]
