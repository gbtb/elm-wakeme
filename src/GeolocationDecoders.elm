module GeolocationDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Geolocation exposing (Altitude, Location, Movement(..), MovingData, posixDecoder)


altitudeDecoder : JD.Decoder Altitude
altitudeDecoder =
    JD.succeed Altitude
        |> JD.required "value" JD.float
        |> JD.required "accuracy" JD.float


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
