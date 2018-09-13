port module Geolocation exposing (Altitude, Destination, Location, Movement(..), MovingData, decodeLatLng, encodeLatLng, posixDecoder, posixEncoder)

import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Maps.Geo exposing (LatLng, latLng)
import Time exposing (Posix)


{-| All available details of the device's current location in the world.

  - `latitude` &mdash; the latitude in decimal degrees.
  - `longitude` &mdash; the longitude in decimal degrees.
  - `accuracy` &mdash; the accuracy of the latitude and longitude, expressed in meters.
  - `altitude` &mdash; altitude information, if available.
  - `movement` &mdash; information about how the device is moving, if available.
  - `timestamp` &mdash; the time that this location reading was taken in milliseconds.

-}
type alias Location =
    { latitude : Float
    , longitude : Float
    , accuracy : Float
    , altitude : Maybe Altitude
    , movement : Maybe Movement
    , timestamp : Posix
    }


{-| The altitude in meters relative to sea level is held in `value`. The `accuracy` field
describes how accurate `value` is, also in meters.
-}
type alias Altitude =
    { value : Float
    , accuracy : Float
    }


{-| Describes the motion of the device. If the device is not moving, this will
just be `Static`. If the device is moving, you will see the `speed` in meters
per second and the `degreesFromNorth` in degrees.
**Note:** The `degreesFromNorth` value goes clockwise: 0° represents true
north, 90° is east, 180° is south, 270° is west, etc.
-}
type Movement
    = Static
    | Moving MovingData


type alias MovingData =
    { speed : Float, degreesFromNorth : Float }


posixDecoder : JD.Decoder Posix
posixDecoder =
    JD.map Time.millisToPosix JD.int


posixEncoder : Posix -> JE.Value
posixEncoder value =
    Time.posixToMillis value |> JE.int


type alias Destination =
    { desiredLocation : LatLng
    , radius : Float
    , name : String
    }


encodeLatLng : LatLng -> JE.Value
encodeLatLng a =
    JE.object
        [ ( "lat", JE.float a.lat )
        , ( "lng", JE.float a.lng )
        ]


decodeLatLng : JD.Decoder LatLng
decodeLatLng =
    JD.map2 latLng (JD.field "lat" JD.float) (JD.field "lng" JD.float)
