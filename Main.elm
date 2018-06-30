module Main exposing (..)

import Html exposing (..)
import Geolocation exposing (..)
import Maps


type alias Model =
    { desiredLocation : Location
    , currentLocation : Location
    , map : Maps.Model Msg
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = NoOp
    | MapsMsg (Maps.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MapsMsg msg ->
            let
                ( updatedMap, cmds ) =
                    Maps.update msg model.map
            in
                ( { model | map = updatedMap }, Cmd.map MapsMsg cmds )


view : Model -> Html Msg
view model =
    div []
        [ text "New Html Program"
        , Html.map MapsMsg <| Maps.view model.map
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


defaultModel =
    { currentLocation = defaultLocation
    , desiredLocation = defaultLocation
    , map = Maps.defaultModel
    }


defaultLocation =
    { latitude = 0.0
    , longitude = 0.0
    , accuracy = 0.0
    , altitude = Nothing
    , movement = Nothing
    , timestamp = 0.0
    }
