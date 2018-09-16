port module Ports exposing (IncomingMsg(..), OutgoingMsg(..), incomingPort, outgoingPort, valueDecoder, valueEncoder)

import Geolocation exposing (Location, posixDecoder)
import Json.Decode as JD
import Json.Encode as JE exposing (Value)


port incomingPort : (Value -> msg) -> Sub msg


port outgoingPort : Value -> Cmd msg


valueEncoder : Value -> JE.Value
valueEncoder a =
    a


valueDecoder : JD.Decoder Value
valueDecoder =
    JD.value


type OutgoingMsg
    = GetCurrentPosition
    | StartAlarm
    | StopAlarm
    | SaveData String Value
    | GetData String


type IncomingMsg
    = LocationUpdate Location
    | LocationUpdateError String
    | NotificationPermissionError
    | AlarmWasStopped
    | ReceiveData String Value
