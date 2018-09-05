port module Ports exposing (IncomingMsg(..), OutgoingMsg(..), incomingPort, outgoingPort)

import Geolocation exposing (Location)
import Json.Encode exposing (Value)


port incomingPort : (Value -> msg) -> Sub msg


port outgoingPort : Value -> Cmd msg


type OutgoingMsg
    = GetCurrentPosition
    | StartAlarm
    | StopAlarm


type IncomingMsg
    = LocationUpdate Location
    | LocationUpdateError String
    | AlarmWasStopped
