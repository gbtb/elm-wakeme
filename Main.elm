module Main exposing (..)

import Html exposing (..)
import Geolocation exposing (..)
import Maps
import Maps.Geo
import Maps.Map
import Maps.Marker
import Maps.Convert
import Mouse


type alias Model =
    { desiredLocation : Location
    , currentLocation : Location
    , map : Maps.Model Data
    , clientPos : ( Float, Float )
    , offsetPos : ( Float, Float )
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Data =
    Maybe Maps.Map.Map


type Msg
    = NoOp
    | MapsMsg (Maps.Msg Data)
    | LocationUpdated Location
    | ClickOnMap Mouse.Event


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

        LocationUpdated newLocation ->
            let
                pos =
                    Maps.Geo.latLng newLocation.latitude newLocation.longitude

                updatedMap =
                    model.map
                        |> Maps.updateMap (Maps.Map.viewBounds <| Maps.Geo.centeredBounds 10 pos)
                        |> Maps.updateMarkers (\_ -> [ Maps.Marker.createCustom (Html.text "*") pos ])
            in
                if newLocation |> isDifferentFrom 0.01 model.currentLocation then
                    ( { model
                        | currentLocation = newLocation
                        , map = updatedMap
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )

        ClickOnMap event ->
            let
                markerPos =
                    Maps.Convert.screenOffsetToLatLng (Maps.Convert.getMap model.map) { x = Tuple.first event.clientPos, y = Tuple.second event.clientPos }
            in
                ( { model
                    | clientPos = event.pagePos
                    , offsetPos = event.offsetPos
                    , map = model.map |> Maps.updateMarkers (\_ -> [ Maps.Marker.createCustom (Html.text "*") markerPos ])
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    div []
        [ text "New Html Program"
        , text <| viewTup model.clientPos
        , text <| viewTup model.offsetPos
        , Html.div [ Mouse.onClick ClickOnMap ] [ Html.map MapsMsg <| Maps.view model.map ]
        ]


viewTup ( a, b ) =
    toString a ++ ":" ++ toString b ++ "\n"


subscriptions : Model -> Sub Msg
subscriptions model =
    changes LocationUpdated


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


defaultModel =
    { currentLocation = defaultLocation
    , desiredLocation = defaultLocation
    , map = Maps.defaultModel
    , clientPos = ( 0, 0 )
    , offsetPos = ( 0, 0 )
    }


defaultLocation =
    { latitude = 0.0
    , longitude = 0.0
    , accuracy = 0.0
    , altitude = Nothing
    , movement = Nothing
    , timestamp = 0.0
    }


isDifferentFrom threshold oldLocation newLocation =
    let
        difLat =
            oldLocation.latitude - newLocation.latitude

        difLong =
            oldLocation.longitude - newLocation.longitude

        metric =
            sqrt <| difLat * difLat + difLong * difLong
    in
        metric > threshold
