module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events
import Geolocation exposing (..)
import Maps
import Maps.Geo
import Maps.Map
import Maps.Marker
import Maps.Convert
import Browser.Events
import Element
import Json.Decode
import List.Extra as Lis
import Html.Events.Extra.Mouse as Mouse


type alias Model =
    { desiredLocation : Location
    , currentLocation : Location
    , map : Maps.Model Data
    , clientPos : ( Float, Float )
    , offsetPos : ( Float, Float )
    , topPos : Float
    }


main : Program Never Model Msg
main =
    Browser.application
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
    | TopClick Mouse.Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msgArg model =
    case msgArg of
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
                x =
                    Tuple.first event.clientPos

                y =
                    model.topPos - Tuple.second event.clientPos

                markerPos =
                    Maps.Convert.screenOffsetToLatLng (Maps.Convert.getMap model.map) { x = x, y = y }
            in
                ( { model
                    | clientPos = event.pagePos
                    , map = model.map |> Maps.updateMarkers (\_ -> [ Maps.Marker.createCustom (Html.text "*") markerPos ])
                  }
                , Cmd.none
                )

        TopClick ev ->
            ( { model | topPos = Tuple.second ev.clientPos }, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [ Element.htmlAttribute <| onClick TopClick ]
            [ Element.text "New Html Program"
            , Element.text <| viewTup model.clientPos
            , Element.text <| viewTup model.offsetPos
            , Element.el [ Element.htmlAttribute <| onClick ClickOnMap ] <| Element.html <| Html.map MapsMsg <| Maps.view model.map
            ]


onClick =
    { stopPropagation = False, preventDefault = False }
        |> Mouse.onWithOptions "click"


viewTup ( a, b ) =
    Debug.toString a ++ ":" ++ Debug.toString b ++ "\n"


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
    , topPos = 0
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
