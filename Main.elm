module Main exposing (Data, Model, Msg(..), defaultLocation, defaultModel, getCurrentPosition, init, isDifferentFrom, main, onClick, onTouch, pixelLength, positionMarker, refreshTargetMarker, subscriptions, targetMarker, update, updateMarker, view, viewAudio, viewRadiusSlider)

import Array
import Browser
import Browser.Dom exposing (getElement, getViewport)
import Browser.Events
import Dict
import Element exposing (fill, maximum)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Geolocation exposing (..)
import Html exposing (..)
import Html.Attributes exposing (id)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Id exposing (Id)
import Json.Decode as JD
import Json.Encode as JE
import Maps
import Maps.Convert
import Maps.Geo exposing (LatLng, latLng)
import Maps.Internal.Maps exposing (Msg(..))
import Maps.Map
import Maps.Marker
import Ports exposing (..)
import PortsDecodersAndEncoders exposing (..)
import Random
import Result.Extra as Result
import Round
import Svg
import Svg.Attributes as Svg
import Task
import Time


type alias Model =
    { currentLocation : Location
    , distance : Float
    , alarmRunning : Bool
    , enabled : Bool
    , map : Maps.Model Data
    , topPos : Float
    , destinations : Dict.Dict String Destination
    , currentDestination : Id
    , editedDestination : Maybe Id
    , seed : Random.Seed
    , menuExpanded : Bool
    , error : Maybe Error
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


type alias Error =
    { text : String
    , action : Maybe Msg
    }


type Msg
    = NoOp
    | MapsMsg (Maps.Msg Data)
    | UpdateLocation
    | ClickOnMap Mouse.Event
    | UpdateMapWindowPosition (Result Browser.Dom.Error Browser.Dom.Element)
    | GetViewport (Result Browser.Dom.Error Browser.Dom.Viewport)
    | GetResizeEvent Int Int
    | RadiusChange Float
    | PortMsg IncomingMsg
    | AddError Error
    | EnableTarget Bool
    | SetInitialSeed Random.Seed
    | ExpandMenu Mouse.Event
    | AddDestination Mouse.Event
    | ChangeDestination Id.Id Mouse.Event
    | EditName Id.Id Mouse.Event
    | DeleteDestination Id.Id Mouse.Event
    | NameChanged String
    | FinishEdit Mouse.Event
    | ClearErrors


update : Msg -> Model -> ( Model, Cmd Msg )
update msgArg model =
    case msgArg of
        NoOp ->
            ( model, Cmd.none )

        SetInitialSeed seed ->
            ( { model | seed = seed }, Cmd.none ) |> updateInitialDestination

        UpdateMapWindowPosition res ->
            case res of
                Ok elem ->
                    ( { model | topPos = elem.element.y }, Cmd.none )

                Err (Browser.Dom.NotFound e) ->
                    ( { model | error = Just { text = e, action = Nothing } }, Cmd.none )

        GetViewport res ->
            case res of
                Ok viewport ->
                    ( { model | map = model.map |> Maps.updateMap (Maps.Map.setWidth (viewport.viewport.width - 20.0)) }, Cmd.none )

                Err (Browser.Dom.NotFound e) ->
                    ( { model | error = Just { text = e, action = Nothing } }, Cmd.none )

        GetResizeEvent width _ ->
            ( { model | map = model.map |> Maps.updateMap (Maps.Map.setWidth (toFloat width - 20.0)) }, Cmd.none )

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
            ( { model | error = Nothing }, getCurrentPosition )

        ClearErrors ->
            ( { model | error = Nothing }, Cmd.none )

        AddError e ->
            ( { model | error = Just e }, Cmd.none )

        ClickOnMap event ->
            let
                x =
                    Tuple.first event.clientPos

                y =
                    Tuple.second event.clientPos - model.topPos

                markerPos =
                    Maps.Convert.screenOffsetToLatLng (Maps.Convert.getMap model.map) { x = x, y = y }

                map =
                    Maps.Convert.getMap model.map

                metersPerPx =
                    pixelLength map.center.lat map.zoom

                marker =
                    targetMarker (model |> getDest) metersPerPx
            in
            ( { model
                | map = model.map |> Maps.updateMarkers (updateMarker 1 (Maps.Marker.createCustom marker markerPos))
              }
                |> updateDest (Maybe.map (\dest -> { dest | desiredLocation = markerPos }))
            , Cmd.none
            )
                |> updateDistance
                |> saveData

        RadiusChange r ->
            ( model
                |> updateDest (Maybe.map (\dest -> { dest | radius = r }))
                |> refreshTargetMarker
            , Cmd.none
            )
                |> updateDistance
                |> saveData

        EnableTarget b ->
            let
                cmd =
                    if model.alarmRunning && not b then
                        stopAlarm

                    else
                        Cmd.none
            in
            ( { model | enabled = b, alarmRunning = model.alarmRunning && b }, cmd )

        ExpandMenu _ ->
            ( { model | menuExpanded = not model.menuExpanded }, Cmd.none )

        AddDestination _ ->
            ( createDestination model |> refreshTargetMarker, Cmd.none ) |> saveData

        ChangeDestination id _ ->
            ( { model | currentDestination = id } |> refreshTargetMarker, Cmd.none )

        EditName id _ ->
            ( { model | editedDestination = Just id }, Cmd.none )

        FinishEdit _ ->
            ( { model | editedDestination = Nothing }, Cmd.none ) |> saveData

        NameChanged str ->
            ( model |> updateEditedDest (Maybe.map (\d -> { d | name = str })), Cmd.none )

        DeleteDestination id _ ->
            let
                newDestinations =
                    Dict.remove (Id.toString id) model.destinations
            in
            ( { model
                | destinations = newDestinations
                , currentDestination =
                    if model.currentDestination == id then
                        Dict.keys newDestinations |> List.head |> Maybe.withDefault "" |> Id.fromString

                    else
                        model.currentDestination
              }
            , Cmd.none
            )
                |> saveData


updateInitialDestination : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateInitialDestination ( model, cmd ) =
    if Dict.size model.destinations == 0 then
        ( createDestination model, cmd )

    else
        ( model, cmd )


createDestination model =
    let
        ( id, nextSeed ) =
            Random.step Id.generator model.seed
    in
    { model
        | seed = nextSeed
        , destinations = Dict.insert (Id.toString id) (defaultDest model) model.destinations
        , currentDestination = id
    }


defaultDest model =
    { desiredLocation = latLng 0 0
    , radius = 0.2
    , name = "Destination " ++ String.fromInt (Dict.size model.destinations + 1)
    }


updateOnPortMsg : IncomingMsg -> Model -> ( Model, Cmd Msg )
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

        ReceiveData _ data ->
            JD.decodeValue dataDecoder data
                |> Result.map (\rData -> { model | destinations = rData })
                |> Result.mapError (\e -> Debug.log "e" e)
                |> Result.withDefault model
                |> (\x -> ( x, Cmd.none ))
                |> setCurrentDestination

        LocationUpdateError e ->
            ( { model | error = Just { text = e, action = Just UpdateLocation } }, Cmd.none )

        NotificationPermissionError ->
            let
                txt =
                    "Please, give a permission for notifications!"
            in
            ( { model | error = Just { text = txt, action = Just UpdateLocation } }, Cmd.none )


setCurrentDestination ( model, cmd ) =
    let
        id =
            model.destinations |> Dict.keys |> List.head |> Maybe.map Id.fromString |> Maybe.withDefault model.currentDestination
    in
    ( { model | currentDestination = id } |> refreshTargetMarker, cmd )


updateDistance ( model, cmd ) =
    let
        destination =
            getDest model

        distance =
            haversine (locationToLatLng model.currentLocation) destination.desiredLocation

        dist =
            distance <= destination.radius * 1000

        playCmd =
            if dist && model.enabled && not model.alarmRunning then
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


getDest model =
    Dict.get (Id.toString model.currentDestination) model.destinations |> Maybe.withDefault (defaultDest model)


updateDest updater model =
    let
        destinations =
            Dict.update (Id.toString model.currentDestination) updater model.destinations
    in
    { model | destinations = destinations }


updateEditedDest updater model =
    case model.editedDestination of
        Just id ->
            let
                destinations =
                    Dict.update (Id.toString id) updater model.destinations
            in
            { model | destinations = destinations }

        Nothing ->
            model


refreshTargetMarker model =
    let
        dest =
            getDest model

        map =
            Maps.Convert.getMap model.map

        metersPerPx =
            pixelLength map.center.lat map.zoom

        updatedMap =
            model.map |> Maps.updateMarkers (updateMarker 1 (Maps.Marker.createCustom (targetMarker dest metersPerPx) dest.desiredLocation))
    in
    { model | map = updatedMap }


view : Model -> { title : String, body : List (Html Msg) }
view model =
    let
        attrs =
            if model.menuExpanded then
                [ Element.inFront (viewMenu model) ]

            else
                []
    in
    { title = "Wakeme"
    , body =
        [ Element.layout [] <|
            Element.column []
                [ viewHeader model
                , Element.column
                    ([ Element.width
                        fill
                     , Element.paddingXY 10 20
                     , Element.spacing 20
                     ]
                        ++ attrs
                    )
                    [ viewErrorMsg model
                    , viewCheckbox model
                    , Element.text <| "Distance: " ++ Round.round 0 model.distance ++ " m"
                    , viewRadiusSlider model
                    , viewAudio
                    , Element.el
                        [ Element.htmlAttribute <| onClick ClickOnMap
                        , Element.htmlAttribute (id "mapWindow")
                        , Element.htmlAttribute <| onTouch ClickOnMap
                        ]
                      <|
                        Element.html <|
                            Html.map MapsMsg <|
                                Maps.view model.map
                    ]
                ]
        ]
    }


positionMarker =
    Html.text "➘"


scale x =
    16 * (1 + 1.25 * (x - 1)) |> round


colors =
    { aqua = Element.rgba255 97 201 168 1.0 --#6bfbaf
    , papaya = Element.rgba255 255 238 219 1 -- #FFEEDB
    , silver = Element.rgba255 173 168 182 0.9
    , purple = Element.rgba255 76 59 77 0.9
    , maroon = Element.rgba255 165 56 96 0.9
    , white = Element.rgba255 255 255 255 1
    , gray = Element.rgba255 0 0 0 0.3
    }


viewErrorMsg model =
    case model.error of
        Just err ->
            let
                label =
                    case err.action of
                        Just UpdateLocation ->
                            Element.text "Update Location"

                        _ ->
                            Element.text "Ok"

                onPress =
                    case err.action of
                        Just UpdateLocation ->
                            UpdateLocation

                        _ ->
                            ClearErrors
            in
            Element.row [ Element.width Element.fill, Background.color colors.maroon, Element.padding 5, Element.spaceEvenly ]
                [ Element.text err.text
                , Input.button [] { onPress = Just onPress, label = label }
                ]

        Nothing ->
            Element.none


viewRadiusSlider model =
    let
        radius =
            Dict.get (Id.toString model.currentDestination) model.destinations |> Maybe.map .radius |> Maybe.withDefault 0.2
    in
    if model.menuExpanded then
        Element.text "Radius"

    else
        Input.slider [ Background.color colors.aqua, Element.htmlAttribute <| Html.Attributes.style "z-index" "-1000" ]
            { min = 0.1
            , max = 2
            , thumb = Input.defaultThumb
            , value = radius
            , step = Just 0.1
            , label = Input.labelLeft [] <| Element.text <| "Radius, " ++ String.fromFloat radius ++ " km"
            , onChange = RadiusChange
            }


viewHeader model =
    let
        destination =
            getDest model

        icon =
            iconHelper "fa-bars"
    in
    Element.row
        [ Element.width Element.fill
        , Background.color colors.papaya
        , Border.shadow { size = 0, offset = ( 0, 4 ), blur = 4, color = colors.silver }
        , Element.paddingXY 10 20
        , Font.size (scale 2)
        ]
        [ Element.el [ Element.centerX, Element.centerY ] <| Element.text destination.name
        , Element.el [ Element.alignRight, mouseOnClick ExpandMenu ] icon
        ]


iconHelper iconName =
    Html.i [ Html.Attributes.classList [ ( "fas", True ), ( iconName, True ) ] ] [] |> Element.html


viewMenu model =
    let
        destinations =
            Dict.toList model.destinations
                |> List.map
                    (\( k, v ) ->
                        let
                            currentlyEdited =
                                model.editedDestination |> Maybe.map Id.toString |> Maybe.withDefault ""
                        in
                        Element.row [ mouseOnClick (ChangeDestination <| Id.fromString k), Element.width fill, Element.spacing 30 ]
                            [ if currentlyEdited /= k then
                                Element.text v.name

                              else
                                Input.text [] { onChange = NameChanged, text = v.name, placeholder = Nothing, label = Input.labelRight [] <| Element.text "" }
                            , if currentlyEdited /= k then
                                Element.el [ Element.alignLeft, mouseOnClick <| EditName <| Id.fromString k ] <| iconHelper "fa-pen"

                              else
                                Element.el [ Element.alignLeft, mouseOnClick <| FinishEdit ] <| iconHelper "fa-check"
                            , if Dict.size model.destinations > 1 then
                                Element.el [ Element.alignRight, mouseOnClick <| DeleteDestination <| Id.fromString k ] <| iconHelper "fa-trash"

                              else
                                Element.none
                            ]
                    )

        addButton =
            Element.row [ Element.width Element.fill, mouseOnClick AddDestination, Font.italic, Font.underline ] [ Element.text "Add destination" ]
    in
    Element.column
        [ Element.height Element.fill
        , Element.width Element.fill
        ]
        [ Element.column
            [ Background.color colors.papaya
            , Element.width <| Element.fill
            , Element.spacingXY 0 20
            , Element.paddingXY 10 20
            ]
            (addButton :: destinations)
        , Element.column [ Element.height <| Element.fill, mouseOnClick ExpandMenu, Element.width fill, Background.color colors.gray ] []
        ]


mouseOnClick x =
    Element.htmlAttribute <| Mouse.onClick x


px =
    Element.px


viewCheckbox model =
    let
        v colorFull shadowOffset alignment =
            Element.el
                [ Element.width (px 60)
                , Element.height (px 20)
                , Background.color colorFull
                , Border.rounded 2
                , Border.innerShadow { offset = ( 0, 0 ), size = 1, blur = 3, color = colors.silver }
                ]
            <|
                Element.el
                    [ Element.width (px 30)
                    , Element.height (px 18)
                    , Element.centerY
                    , alignment
                    , Border.shadow { offset = shadowOffset, size = 1, blur = 2, color = colors.purple }
                    , Background.gradient { angle = 0, steps = [ colors.silver, colors.white, colors.silver ] }
                    ]
                <|
                    Element.none
    in
    Input.checkbox []
        { label = Input.labelLeft [] <| Element.none
        , icon =
            \x ->
                if x then
                    v colors.aqua ( -1, 0 ) Element.alignRight

                else
                    v colors.purple ( 1, 0 ) Element.alignLeft
        , checked = model.enabled
        , onChange = EnableTarget
        }


targetMarker model metersPerPx =
    let
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ incomingPort processIncomingMsg
        , Browser.Events.onResize GetResizeEvent
        ]


init _ =
    ( defaultModel
    , Cmd.batch
        [ Task.attempt GetViewport getViewport
        , Task.attempt UpdateMapWindowPosition <| getElement "mapWindow"
        , Random.generate SetInitialSeed Random.independentSeed
        , getData
        ]
    )


defaultModel =
    let
        dummyMarker =
            Maps.Marker.createCustom (Html.text "") <| Maps.Geo.latLng 0 0
    in
    { currentLocation = defaultLocation
    , distance = 0
    , alarmRunning = False
    , enabled = False
    , map =
        Maps.defaultModel
            |> Maps.updateMarkers (\_ -> List.repeat 2 dummyMarker)
            |> Maps.updateMap (Maps.Map.setWidth 500 >> Maps.Map.setHeight 400)
    , topPos = 0
    , destinations = Dict.empty
    , currentDestination = Id.fromString ""
    , seed = Random.initialSeed 0
    , menuExpanded = False
    , editedDestination = Nothing
    , error = Nothing
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
    JD.decodeValue incomingMsgDecoder value
        |> Result.map PortMsg
        |> Result.mapError (\e -> Debug.log "incoming port error" <| AddError { text = JD.errorToString e, action = Nothing })
        |> Result.merge


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


getData =
    outgoingPort <| outgoingMsgEncoder (GetData "destinations")


saveData ( model, cmd ) =
    ( model
    , Debug.log "batch" <|
        Cmd.batch
            [ Debug.log "cmd" cmd
            , outgoingPort <| outgoingMsgEncoder (SaveData "destinations" <| dataEncoder model)
            ]
    )


dataEncoder model =
    model.destinations |> JE.dict identity destinationEncoder


dataDecoder =
    JD.dict destinationDecoder
