module PortsDecodersAndEncoders exposing (..)

import Geolocation exposing (Altitude, Location, Movement(..), MovingData, posixDecoder, posixEncoder)
import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Ports exposing (IncomingMsg(..), OutgoingMsg(..))


altitudeDecoder : JD.Decoder Altitude
altitudeDecoder =
    JD.succeed Altitude
        |> JD.required "value" JD.float
        |> JD.required "accuracy" JD.float


incomingMsgDecoder : JD.Decoder IncomingMsg
incomingMsgDecoder =
    JD.oneOf
        [ JD.field "LocationUpdate" (JD.map LocationUpdate locationDecoder)
        , JD.field "LocationUpdateError" (JD.map LocationUpdateError JD.string)
        , JD.field "AlarmWasStopped" (JD.succeed AlarmWasStopped)
        ]


locationDecoder : JD.Decoder Location
locationDecoder =
    JD.succeed Location
        |> JD.required "latitude" JD.float
        |> JD.required "longitude" JD.float
        |> JD.required "accuracy" JD.float
        |> JD.required "altitude" (maybeDecoder altitudeDecoder)
        |> JD.required "movement" (maybeDecoder movementDecoder)
        |> JD.required "timestamp" posixDecoder


maybeDecoder decoder =
    JD.oneOf
        [ JD.null Nothing
        , JD.map Just decoder
        ]


movementDecoder : JD.Decoder Movement
movementDecoder =
    JD.oneOf
        [ JD.field "Static" (JD.succeed Static)
        , JD.field "Moving" (JD.map Moving movingDataDecoder)
        ]


movingDataDecoder : JD.Decoder MovingData
movingDataDecoder =
    JD.succeed MovingData
        |> JD.required "speed" JD.float
        |> JD.required "degreesFromNorth" JD.float


outgoingMsgDecoder : JD.Decoder OutgoingMsg
outgoingMsgDecoder =
    JD.oneOf
        [ JD.field "GetCurrentPosition" (JD.succeed GetCurrentPosition)
        , JD.field "StartAlarm" (JD.succeed StartAlarm)
        , JD.field "StopAlarm" (JD.succeed StopAlarm)
        ]


altitudeEncoder : Altitude -> JE.Value
altitudeEncoder value =
    JE.object
        [ ( "value", JE.float value.value )
        , ( "accuracy", JE.float value.accuracy )
        ]


incomingMsgEncoder : IncomingMsg -> JE.Value
incomingMsgEncoder value =
    case value of
        LocationUpdate v1 ->
            JE.object [ ( "LocationUpdate", locationEncoder v1 ) ]

        LocationUpdateError v1 ->
            JE.object [ ( "LocationUpdateError", JE.string v1 ) ]

        AlarmWasStopped ->
            JE.object [ ( "AlarmWasStopped", JE.null ) ]


locationEncoder : Location -> JE.Value
locationEncoder value =
    JE.object
        [ ( "latitude", JE.float value.latitude )
        , ( "longitude", JE.float value.longitude )
        , ( "accuracy", JE.float value.accuracy )
        , ( "altitude", maybeEncoder altitudeEncoder value.altitude )
        , ( "movement", maybeEncoder movementEncoder value.movement )
        , ( "timestamp", posixEncoder value.timestamp )
        ]


maybeEncoder valueEncoder valueArg =
    case valueArg of
        Just value ->
            valueEncoder value

        Nothing ->
            JE.null


movementEncoder : Movement -> JE.Value
movementEncoder value =
    case value of
        Static ->
            JE.object [ ( "Static", JE.null ) ]

        Moving v1 ->
            JE.object [ ( "Moving", movingDataEncoder v1 ) ]


movingDataEncoder : MovingData -> JE.Value
movingDataEncoder value =
    JE.object
        [ ( "speed", JE.float value.speed )
        , ( "degreesFromNorth", JE.float value.degreesFromNorth )
        ]


outgoingMsgEncoder : OutgoingMsg -> JE.Value
outgoingMsgEncoder value =
    case value of
        GetCurrentPosition ->
            JE.object [ ( "GetCurrentPosition", JE.null ) ]

        StartAlarm ->
            JE.object [ ( "StartAlarm", JE.null ) ]

        StopAlarm ->
            JE.object [ ( "StopAlarm", JE.null ) ]
