module PortsDecodersAndEncoders exposing (altitudeDecoder, altitudeEncoder, destinationDecoder, destinationEncoder, incomingMsgDecoder, incomingMsgEncoder, locationDecoder, locationEncoder, maybeDecoder, maybeEncoder, movementDecoder, movementEncoder, movingDataDecoder, movingDataEncoder, outgoingMsgDecoder, outgoingMsgEncoder)

import Geolocation exposing (Altitude, Destination, Location, Movement(..), MovingData, decodeLatLng, encodeLatLng, posixDecoder, posixEncoder)
import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Ports exposing (IncomingMsg(..), OutgoingMsg(..), valueDecoder, valueEncoder)


altitudeDecoder : JD.Decoder Altitude
altitudeDecoder =
    JD.succeed Altitude
        |> JD.required "value" JD.float
        |> JD.required "accuracy" JD.float


destinationDecoder : JD.Decoder Destination
destinationDecoder =
    JD.succeed Destination
        |> JD.required "desiredLocation" decodeLatLng
        |> JD.required "radius" JD.float
        |> JD.required "name" JD.string


incomingMsgDecoder : JD.Decoder IncomingMsg
incomingMsgDecoder =
    JD.oneOf
        [ JD.field "LocationUpdate" (JD.map LocationUpdate locationDecoder)
        , JD.field "LocationUpdateError" (JD.map LocationUpdateError JD.string)
        , JD.field "AlarmWasStopped" (JD.succeed AlarmWasStopped)
        , JD.field "ReceiveData" (JD.map2 ReceiveData (JD.index 0 JD.string) (JD.index 1 valueDecoder))
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
        , JD.field "SaveData" (JD.map2 SaveData (JD.index 0 JD.string) (JD.index 1 valueDecoder))
        , JD.field "GetData" (JD.map GetData JD.string)
        ]


altitudeEncoder : Altitude -> JE.Value
altitudeEncoder value =
    JE.object
        [ ( "value", JE.float value.value )
        , ( "accuracy", JE.float value.accuracy )
        ]


destinationEncoder : Destination -> JE.Value
destinationEncoder value =
    JE.object
        [ ( "desiredLocation", encodeLatLng value.desiredLocation )
        , ( "radius", JE.float value.radius )
        , ( "name", JE.string value.name )
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

        ReceiveData v1 v2 ->
            JE.object [ ( "ReceiveData", JE.list identity [ JE.string v1, valueEncoder v2 ] ) ]


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

        SaveData v1 v2 ->
            JE.object [ ( "SaveData", JE.list identity [ JE.string v1, valueEncoder v2 ] ) ]

        GetData v1 ->
            JE.object [ ( "GetData", JE.string v1 ) ]
