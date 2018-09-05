module Main exposing (Data, Model, Msg(..), c, defaultLocation, defaultModel, getCurrentPosition, init, isDifferentFrom, main, onClick, onTouch, pixelLength, positionMarker, refreshTargetMarker, subscriptions, targetMarker, update, updateMarker, view, viewAudio, viewRadiusSlider, viewTup)

import Array
import Browser
import Browser.Dom exposing (getElement)
import Browser.Events
import Element exposing (fill, maximum)
import Element.Input as Input
import Geolocation exposing (..)
import GeolocationDecoders exposing (locationDecoder)
import Html exposing (..)
import Html.Attributes exposing (id)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode as JD
import Maps
import Maps.Convert
import Maps.Geo exposing (LatLng, latLng)
import Maps.Internal.Maps exposing (Msg(..))
import Maps.Map
import Maps.Marker
import Ports exposing (..)
import PortsDecodersAndEncoders exposing (..)
import Svg
import Svg.Attributes as Svg
import Task
import Time


type alias Model =
    { desiredLocation : LatLng
    , currentLocation : Location
    , map : Maps.Model Data
    , clientPos : ( Float, Float )
    , offsetPos : ( Float, Float )
    , topPos : Float
    , radius : Float
    }


main : Program () Model Msg
main =
    Browser.document
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
    | UpdateLocation
    | ClickOnMap Mouse.Event
    | UpdateMapWindowPosition (Result Browser.Dom.Error Browser.Dom.Element)
    | RadiusChange Float
    | PortMsg IncomingMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msgArg model =
    case msgArg of
        NoOp ->
            ( model, Cmd.none )

        UpdateMapWindowPosition res ->
            case res of
                Ok elem ->
                    let
                        e =
                            Debug.log "e" elem
                    in
                    ( { model | topPos = e.element.y }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        MapsMsg msg ->
            let
                ( updatedMap, cmds ) =
                    Maps.update msg model.map

                --zoom, pinch
            in
            case msg of
                Zoom _ val ->
                    let
                        oldZoom =
                            Maps.Convert.getMap model.map |> .zoom

                        newModel =
                            { model | map = updatedMap }
                                |> (if floor (oldZoom + val) /= floor oldZoom then
                                        refreshTargetMarker

                                    else
                                        identity
                                   )
                    in
                    ( newModel, Cmd.map MapsMsg cmds )

                _ ->
                    ( { model | map = updatedMap }, Cmd.map MapsMsg cmds )

        PortMsg msg ->
            updateOnPortMsg msg model

        UpdateLocation ->
            ( model, getCurrentPosition )

        ClickOnMap event ->
            let
                e =
                    Debug.log "e" event

                x =
                    Tuple.first event.clientPos

                y =
                    Tuple.second event.clientPos - model.topPos

                --- Tuple.second event.offsetPos
                markerPos =
                    Maps.Convert.screenOffsetToLatLng (Maps.Convert.getMap model.map) { x = x, y = y }

                marker =
                    targetMarker model
            in
            ( { model
                | clientPos = event.clientPos
                , offsetPos = ( x, y )
                , map = model.map |> Maps.updateMarkers (updateMarker 1 (Maps.Marker.createCustom marker markerPos))
                , desiredLocation = markerPos
              }
            , Cmd.none
            )

        RadiusChange r ->
            ( { model | radius = r } |> refreshTargetMarker, Cmd.none )


updateOnPortMsg msg model =
    case msg of
        LocationUpdate newLocation ->
            let
                pos =
                    Maps.Geo.latLng newLocation.latitude newLocation.longitude

                updatedMap =
                    model.map
                        |> Maps.updateMap (Maps.Map.viewBounds <| Maps.Geo.centeredBounds 10 pos)
                        |> Maps.updateMarkers (updateMarker 0 (Maps.Marker.createCustom positionMarker pos))
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

        _ ->
            ( model, Cmd.none )


updateMarker idx marker markers =
    Array.fromList markers
        |> Array.set idx marker
        |> Array.toList


refreshTargetMarker model =
    let
        updatedMap =
            model.map |> Maps.updateMarkers (updateMarker 1 (Maps.Marker.createCustom (targetMarker model) model.desiredLocation))
    in
    { model | map = updatedMap }


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Wakeme"
    , body =
        [ Element.layout [] <|
            Element.column
                [ Element.width
                    (fill
                        |> maximum 300
                    )
                ]
                [ Input.button []
                    { onPress = Just UpdateLocation
                    , label = Element.text "Update Location"
                    }
                , Element.text <| viewTup model.clientPos
                , Element.text <| viewTup model.offsetPos
                , viewRadiusSlider model
                , Element.el
                    [ Element.htmlAttribute <| onClick ClickOnMap
                    , Element.htmlAttribute (id "mapWindow")
                    , Element.htmlAttribute <| onTouch ClickOnMap
                    , Element.htmlAttribute (id "mapWindow")
                    ]
                  <|
                    Element.html <|
                        Html.map MapsMsg <|
                            Maps.view model.map
                ]
        ]
    }


positionMarker =
    Html.text "➘"


viewRadiusSlider model =
    Input.slider []
        { min = 0.1
        , max = 2
        , thumb = Input.defaultThumb
        , value = model.radius
        , step = Just 0.1
        , label = Input.labelLeft [] <| Element.text <| "Radius, " ++ String.fromFloat model.radius ++ " km"
        , onChange = RadiusChange
        }


targetMarker model =
    let
        map =
            Maps.Convert.getMap model.map

        metersPerPx =
            pixelLength map.center.lat map.zoom

        radius =
            model.radius * 1000 / metersPerPx |> round

        a =
            radius * 2 |> String.fromInt
    in
    Svg.svg
        [ Svg.width a
        , Svg.height a
        , Svg.viewBox <| "0 0 " ++ a ++ " " ++ a
        ]
        [ Svg.circle [ Svg.cx (String.fromInt radius), Svg.cy (String.fromInt radius), Svg.r <| String.fromInt radius, Svg.opacity "0.25" ] [] ]


viewAudio =
    Html.audio [ Html.Attributes.src "alarm.mp3" ] []


onClick =
    { stopPropagation = False, preventDefault = False }
        |> Mouse.onWithOptions "click"


onTouch =
    { stopPropagation = False, preventDefault = True }
        |> Mouse.onWithOptions "touchend"


viewTup ( a, b ) =
    Debug.toString a ++ ":" ++ Debug.toString b ++ "\n"


subscriptions : Model -> Sub Msg
subscriptions model =
    incomingPort processIncomingMsg


init _ =
    ( defaultModel, Task.attempt UpdateMapWindowPosition <| getElement "mapWindow" )


defaultModel =
    let
        dummyMarker =
            Maps.Marker.createCustom (Html.text "") <| Maps.Geo.latLng 0 0
    in
    { currentLocation = defaultLocation
    , desiredLocation = latLng 0 0
    , map =
        Maps.defaultModel
            |> Maps.updateMarkers (\_ -> List.repeat 2 dummyMarker)
            |> Maps.updateMap (Maps.Map.setWidth 500 >> Maps.Map.setHeight 400)
    , clientPos = ( 0, 0 )
    , offsetPos = ( 0, 0 )
    , topPos = 0
    , radius = 0.1
    }


defaultLocation =
    { latitude = 0.0
    , longitude = 0.0
    , accuracy = 0.0
    , altitude = Nothing
    , movement = Nothing
    , timestamp = Time.millisToPosix 0
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


processIncomingMsg value =
    JD.decodeValue incomingMsgDecoder value |> Result.map PortMsg |> Result.withDefault NoOp



--taken from https://wiki.openstreetmap.org/wiki/Zoom_levels


c =
    pi * 6378137


pixelLength lat zoomLevel =
    c * cos (degrees lat) / (2 ^ (zoomLevel + 8))


getCurrentPosition =
    outgoingPort <| outgoingMsgEncoder GetCurrentPosition
