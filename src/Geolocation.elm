port module Geolocation exposing (..)

import Time exposing (Posix)
import Json.Encode exposing (Value)


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
    | Moving { speed : Float, degreesFromNorth : Float }


port changes : (Value -> msg) -> Sub msg
