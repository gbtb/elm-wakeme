module Main exposing (Data, Model, Msg(..), defaultLocation, defaultModel, getCurrentPosition, init, isDifferentFrom, main, onClick, onTouch, pixelLength, positionMarker, refreshTargetMarker, subscriptions, targetMarker, update, updateMarker, view, viewAudio, viewRadiusSlider, viewTup)

import Array
import Browser
import Browser.Dom exposing (getElement)
import Browser.Events
import Element exposing (fill, maximum)
import Element.Background as Background
import Element.Border as Border
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
import Round
import Svg
import Svg.Attributes as Svg
import Task
import Time


type alias Model =
    { desiredLocation : LatLng
    , currentLocation : Location
    , distance : Float
    , alarmRunning : Bool
    , enabled : Bool
    , map : Maps.Model Data
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
    | EnableTarget Bool


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
                x =
                    Tuple.first event.clientPos

                y =
                    Tuple.second event.clientPos - model.topPos

                markerPos =
                    Maps.Convert.screenOffsetToLatLng (Maps.Convert.getMap model.map) { x = x, y = y }

                marker =
                    targetMarker model
            in
            ( { model
                | map = model.map |> Maps.updateMarkers (updateMarker 1 (Maps.Marker.createCustom marker markerPos))
                , desiredLocation = markerPos
              }
            , Cmd.none
            )
                |> updateDistance

        RadiusChange r ->
            ( { model | radius = r } |> refreshTargetMarker, Cmd.none ) |> updateDistance

        EnableTarget b ->
            let
                cmd =
                    if model.alarmRunning && not b then
                        stopAlarm

                    else
                        Cmd.none
            in
            ( { model | enabled = b }, cmd )


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
                    |> updateDistance

            else
                ( model, Cmd.none )

        AlarmWasStopped ->
            ( { model | alarmRunning = False }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateDistance ( model, cmd ) =
    let
        distance =
            haversine (locationToLatLng model.currentLocation) model.desiredLocation

        dist =
            distance <= model.radius * 1000

        playCmd =
            if dist && not model.alarmRunning then
                playAlarm

            else
                Cmd.none
    in
    ( { model
        | distance = distance
        , alarmRunning = model.alarmRunning || dist
      }
    , Cmd.batch [ cmd, playCmd ]
    )


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
        [ Element.layout [ Background.color colors.papaya ] <|
            Element.column
                [ Element.width
                    (fill
                        |> maximum 300
                    )
                , Element.padding 10
                , Element.spacing 20
                ]
                [ viewCheckbox model
                , Element.text <| "distance: " ++ Round.round 0 model.distance ++ " m"
                , viewRadiusSlider model
                , viewAudio
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


colors =
    { aqua = Element.rgba255 97 201 168 1.0
    , papaya = Element.rgba255 255 238 219 0.5
    }


viewRadiusSlider model =
    Input.slider [ Background.color colors.aqua ]
        { min = 0.1
        , max = 2
        , thumb = Input.defaultThumb
        , value = model.radius
        , step = Just 0.1
        , label = Input.labelLeft [] <| Element.text <| "Radius, " ++ String.fromFloat model.radius ++ " km"
        , onChange = RadiusChange
        }


viewCheckbox model =
    Input.checkbox []
        { label = Input.labelLeft [] <| Element.text "Enabled"
        , icon =
            \x ->
                if x then
                    Element.text "x"

                else
                    Element.text "o"
        , checked = model.enabled
        , onChange = EnableTarget
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
    Element.html <|
        Html.audio [ Html.Attributes.src "alarm.mp3", Html.Attributes.id "alarm", Html.Attributes.attribute "loop" "true" ] []


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
    , distance = 0
    , alarmRunning = False
    , enabled = False
    , map =
        Maps.defaultModel
            |> Maps.updateMarkers (\_ -> List.repeat 2 dummyMarker)
            |> Maps.updateMap (Maps.Map.setWidth 500 >> Maps.Map.setHeight 400)
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


locationToLatLng loc =
    latLng loc.latitude loc.longitude



--taken from https://wiki.openstreetmap.org/wiki/Zoom_levels


circ =
    pi * 6378137


pixelLength lat zoomLevel =
    circ * cos (degrees lat) / (2 ^ (zoomLevel + 8))


haversine : LatLng -> LatLng -> Float
haversine p1 p2 =
    let
        sqSin getter =
            sin (degrees <| (getter p1 - getter p2) / 2) ^ 2

        a =
            sqSin .lat + cos (degrees p1.lat) * cos (degrees p2.lat) * sqSin .lng

        c =
            2 * atan2 (sqrt a) (sqrt (1 - a))

        r =
            6371 * 1000
    in
    r * c


getCurrentPosition =
    outgoingPort <| outgoingMsgEncoder GetCurrentPosition


playAlarm =
    outgoingPort <| outgoingMsgEncoder StartAlarm


stopAlarm =
    outgoingPort <| outgoingMsgEncoder StopAlarm
