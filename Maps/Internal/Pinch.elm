module Maps.Internal.Pinch
    exposing
        ( Pinch
        , start
        , pinch
        , startEnd
        )

import Maps.Internal.Screen as Screen exposing (TwoFingers)


type Pinch
    = StartPinch TwoFingers
    | Pinch TwoFingers TwoFingers


start : TwoFingers -> Pinch
start =
    StartPinch


pinch : TwoFingers -> Pinch -> Pinch
pinch twoFingers state =
    case state of
        StartPinch startVal ->
            Pinch startVal twoFingers

        Pinch startVal end ->
            Pinch startVal twoFingers


startEnd : Pinch -> ( TwoFingers, TwoFingers )
startEnd pinchArg =
    case pinchArg of
        StartPinch startVal ->
            ( startVal, startVal )

        Pinch startVal end ->
            ( startVal, end )
