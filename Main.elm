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
import Browser.Dom exposing (getElement)
import Element exposing (maximum, fill)
import Element.Input as Input
import Json.Decode
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Time
import Json.Decode as JD
import GeolocationDecoders exposing (locationDecoder)
import Task
import Html.Attributes exposing (id)
import Maps.Geo exposing (LatLng, latLng)
import Array
import Svg
import Svg.Attributes as Svg

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
    | LocationUpdated (Result JD.Error Location)
    | UpdateLocation 
    | ClickOnMap Mouse.Event
    | UpdateMapWindowPosition (Result Browser.Dom.Error Browser.Dom.Element)
    | RadiusChange Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msgArg model =
    case msgArg of
        NoOp ->
            ( model, Cmd.none )

        UpdateMapWindowPosition res ->
            case res of
                Ok elem -> 
                    let e = (Debug.log "e" elem) in
                    ({model | topPos =  e.element.y}, Cmd.none)

                Err e ->
                    (model, Cmd.none)


        MapsMsg msg ->
            let
                ( updatedMap, cmds ) =
                    Maps.update msg model.map
            in
                ( { model | map = updatedMap }, Cmd.map MapsMsg cmds )

        LocationUpdated locationRes ->
            case locationRes of
                Ok newLocation ->
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
                
                Err e -> Debug.log (JD.errorToString e) ( model, Cmd.none )

        UpdateLocation -> (model, getCurrentPosition () )

        ClickOnMap event ->
            let

                e = Debug.log "e" event
                x =
                    Tuple.first event.clientPos

                y =
                    Tuple.second event.clientPos - model.topPos --- Tuple.second event.offsetPos

                markerPos =
                    Maps.Convert.screenOffsetToLatLng (Maps.Convert.getMap model.map) { x = x, y = y }

                marker = targetMarker model
            in
                ( { model
                    | clientPos = event.clientPos,
                    offsetPos = (x,y)
                    , map = model.map |> Maps.updateMarkers (updateMarker 1 (Maps.Marker.createCustom marker markerPos))
                    , desiredLocation = markerPos
                  }
                , Cmd.none
                )

        RadiusChange r ->
            ({model | radius = r, map = refreshTargetMarker model}, Cmd.none)


updateMarker idx marker markers = Array.fromList markers |>
    Array.set idx marker |> Array.toList


refreshTargetMarker model = model.map |> Maps.updateMarkers (updateMarker 1 (Maps.Marker.createCustom (targetMarker model) model.desiredLocation))


view : Model -> {title: String, body: List (Html Msg)}
view model = {
    title = "Wakeme",
    body = [Element.layout [] <|
        Element.column [ Element.width (fill
                    |> maximum 300) ]
            [ Input.button [] { onPress = Just UpdateLocation
                    , label = Element.text "Update Location"
                    }
            , Element.text <| viewTup model.clientPos
            , Element.text <| viewTup model.offsetPos
            , viewRadiusSlider model
            , Element.el [ Element.htmlAttribute <| onClick ClickOnMap, Element.htmlAttribute (id "mapWindow"),
            Element.htmlAttribute <| onTouch ClickOnMap, Element.htmlAttribute (id "mapWindow")
             ] <| Element.html <| Html.map MapsMsg <| Maps.view model.map
            ]]
    }
    

positionMarker = Html.text "➘"

viewRadiusSlider model = Input.slider [] {
    min = 0.1,
    max = 10,
    thumb = Input.defaultThumb,
    value = model.radius,
    step = Just 0.1,
    label = Input.labelLeft [] <| Element.text <| "Radius, " ++ String.fromFloat model.radius ++ " km",
    onChange = RadiusChange
    }

targetMarker model =
    let 
        map = Maps.Convert.getMap model.map
        metersPerPx = pixelLength map.center.lat map.zoom
        radius = model.radius * 1000 / metersPerPx
    in
     Svg.svg [ Svg.width "200"
    , Svg.height "200"
    , Svg.viewBox "0 0 200 200"
    ] 
    [Svg.circle [ Svg.cx "100", Svg.cy "100", Svg.r <| String.fromFloat radius, Svg.color "#FFFFFF" ] []]


onClick =
    { stopPropagation = False, preventDefault = False }
        |> Mouse.onWithOptions "click"


onTouch = { stopPropagation = False, preventDefault = True }
        |> Mouse.onWithOptions "touchend"


viewTup ( a, b ) =
    Debug.toString a ++ ":" ++ Debug.toString b ++ "\n"


subscriptions : Model -> Sub Msg
subscriptions model =
    changes processLocation


init _ =
    ( defaultModel, Task.attempt UpdateMapWindowPosition <| getElement "mapWindow" )


defaultModel =
    let dummyMarker = Maps.Marker.createCustom (Html.text "") <| Maps.Geo.latLng 0 0 in
    { currentLocation = defaultLocation
    , desiredLocation = latLng 0 0
    , map = Maps.defaultModel 
        |> Maps.updateMarkers (\_ -> List.repeat 2 dummyMarker)
        |> Maps.updateMap (Maps.Map.setWidth 500 >> Maps.Map.setHeight 400 )
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


processLocation value = JD.decodeValue locationDecoder value |> LocationUpdated


--taken from https://wiki.openstreetmap.org/wiki/Zoom_levels
c = pi * 6378137
pixelLength lat zoomLevel = c * cos (degrees lat) / ( 2 ^ (zoomLevel + 8) )