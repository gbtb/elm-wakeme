module Maps.Internal.Maps
    exposing
        ( Msg(..)
        , Model
        , updateMap
        , updateMarkers
        , defaultModel
        , update
        , subscriptions
        , view
        , mapView
        )

{-| The Maps library contains the functions neccessary for an
[HTML.program](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#program).


# Creating a map

The quickest way to get up and running is to create a map with default options

    import Maps.Internal
    import Html exposing (program)

    main =
        program <| Maps.map Maps.defaultOptions

@docs map
@docs Options
@docs defaultOptions


# Definitions

@docs Msg
@docs Model


# Program functions

@docs update
@docs subscriptions
@docs view

-}

import List.Extra as List
import Json.Decode as Json
import Html exposing (Html)
import Html.Keyed
import Html.Attributes as Attr
import Html.Events exposing (custom)
import Maps.Internal.Map as Map exposing (Map)
import Maps.Internal.Screen as Screen exposing (Offset, TwoFingers, ZoomLevel)
import Maps.Internal.LatLng as LatLng exposing (LatLng)
import Maps.Internal.Bounds as Bounds exposing (Bounds)
import Maps.Internal.Marker as Marker exposing (Marker)
import Maps.Internal.Tile as Tile exposing (Tile)
import Maps.Internal.Drag as Drag exposing (Drag)
import Maps.Internal.Pinch as Pinch exposing (Pinch)
import Maps.Internal.Zoom as Zoom
import Maps.Internal.Utils exposing (noDefaultButPropagate, flip)


{-| The map has events for dragging, zooming and setting the bounds displayed by the map.
-}
type Msg msg
    = DragStart Offset
    | DragTo Offset
    | DragStop
    | PinchStart TwoFingers
    | PinchTo TwoFingers
    | PinchStop
    | Zoom Offset ZoomLevel
    | ExternalMsg msg


{-| The map's model consists of the [properties necessary to display a static map](Maps-Map#Map),
a cache of the previous map (for simulated zooming/panning before the real tiles load in)
and the state of the map being dragged.
-}
type alias Model msg =
    { map : Map
    , cache : List Map
    , markers : List (Marker msg)
    , drag : Maybe Drag
    , pinch : Maybe Pinch
    }


updateMap : (Map -> Map) -> Model msg -> Model msg
updateMap updateFunc model =
    { model
        | map = updateFunc model.map
        , cache = model.map :: model.cache |> List.uniqueBy (.zoom >> ceiling)
    }


updateMarkers : (List (Marker msg) -> List (Marker msg)) -> Model msg -> Model msg
updateMarkers updateFunc model =
    { model
        | markers = updateFunc model.markers
    }


{-| A default model that displays Open Street Map tiles looking at Sydney.
-}
defaultModel : Model msg
defaultModel =
    let
        map =
            { tileServer = "https://a.tile.osm.org/{z}/{x}/{y}.png"
            , zoom = 10
            , center = LatLng.sydney
            , width = 600
            , height = 400
            , tileSize = 256
            }
    in
        { map = map
        , cache = []
        , markers = []
        , drag = Nothing
        , pinch = Nothing
        }


{-| -}
update : Msg msg -> Model msg -> ( Model msg, Cmd (Msg msg) )
update msg model =
    case msg of
        DragStart offset ->
            let
                dragState =
                    if model.pinch == Nothing then
                        Just <| Drag.start offset
                    else
                        Nothing
            in
                ( { model | drag = dragState }, Cmd.none )

        DragTo offset ->
            let
                dragState =
                    if model.pinch == Nothing then
                        Maybe.map (Drag.drag offset) model.drag
                    else
                        Nothing

                draggedMap map =
                    dragState
                        |> Maybe.map (flip Map.drag <| map)
                        |> Maybe.withDefault map
            in
                ( updateMap draggedMap { model | drag = dragState }, Cmd.none )

        DragStop ->
            ( { model | drag = Nothing }, Cmd.none )

        PinchStart fingers ->
            ( { model | pinch = Just <| Pinch.start fingers }, Cmd.none )

        PinchTo fingers ->
            ( { model | pinch = Maybe.map (Pinch.pinch fingers) model.pinch }, Cmd.none )

        PinchStop ->
            let
                zoom =
                    Maybe.map (Zoom.fromPinch model.map.width model.map.height) model.pinch

                pinchedMap map =
                    case zoom of
                        Just ( zoomVal, offset ) ->
                            Map.zoomTo zoomVal offset map

                        Nothing ->
                            map
            in
                ( updateMap pinchedMap { model | pinch = Nothing }, Cmd.none )

        Zoom offset zoom ->
            ( updateMap (Map.zoomTo zoom offset) model, Cmd.none )

        ExternalMsg _ ->
            ( model, Cmd.none )


{-| -}
subscriptions : Model msg -> Sub (Msg msg)
subscriptions map =
    Sub.none


{-| -}
view : Model msg -> Html (Msg msg)
view ({ map, cache, markers, pinch, drag } as model) =
    Html.div
        ([ Attr.style "width" <| String.fromFloat map.width ++ "px"
         , Attr.style "height" <| String.fromFloat map.height ++ "px"
         , Attr.style "-webkit-touch-callout" "none"
         , Attr.style "-webkit-user-select" "none"
         , Attr.style "-khtml-user-select" "none"
         , Attr.style "-moz-user-select" "none"
         , Attr.style "-ms-user-select" "none"
         , Attr.style "user-select" "none"
         , Attr.style "background-color" "#ddd"
         ]
            ++ zoomEvents map.zoom
        )
    <|
        let
            zoom =
                Maybe.map (Zoom.fromPinch map.width map.height) pinch

            zoomedMap =
                Maybe.withDefault map <| Maybe.map (\( zoomArg, offset ) -> Map.zoomTo zoomArg offset map) zoom

            transforms =
                Map.diff zoomedMap map
        in
            [ Html.div
                [ Attr.style "position"
                    "absolute"
                , Attr.style "width" <|
                    String.fromFloat
                        map.width
                        ++ "px"
                , Attr.style "height" <|
                    String.fromFloat
                        map.height
                        ++ "px"
                , Attr.style "overflow"
                    "hidden"
                , custom "mouseDown" <|
                    Json.map noDefaultButPropagate <|
                        Json.fail "No interaction"
                ]
              <|
                List.map (\cachedMap -> tilesView (Map.diff zoomedMap cachedMap) cachedMap) <|
                    List.reverse <|
                        cache
            , Html.div
                ([ Attr.style "position" "absolute"
                 , Attr.style "width" <| String.fromFloat map.width ++ "px"
                 , Attr.style "height" <| String.fromFloat map.height ++ "px"
                 , Attr.style "overflow" "hidden"
                 ]
                    ++ dragEvents drag
                )
                [ tilesView transforms map
                ]
            , Html.div
                [ Attr.style "position" "absolute"
                , Attr.style "width" <| String.fromFloat map.width ++ "px"
                , Attr.style "height" <| String.fromFloat map.height ++ "px"
                , Attr.style "overflow" "hidden"
                , Attr.style "pointer-events" "none"
                ]
              <|
                List.map (Html.map ExternalMsg) <|
                    List.map (Marker.view zoomedMap) <|
                        markers
            ]


{-| Map a Map HTML view to an arbitrary HTML view which wraps map messages.
-}
mapView : (Msg msg -> msg) -> Html (Msg msg) -> Html msg
mapView wrapMsg html =
    let
        mapMsg mapsMsgArg =
            case mapsMsgArg of
                ExternalMsg msg ->
                    msg

                mapsMsg ->
                    wrapMsg mapsMsg
    in
        Html.map mapMsg html


tilesView : List Map.Transformation -> Map -> Html (Msg msg)
tilesView transforms map =
    let
        attrs =
            (List.map (\( prop, val ) -> Attr.style prop val) <|
                Map.transformationStyle map.width map.height <|
                    transforms
            )

        content =
            List.map (\(( url, offset ) as tile) -> ( url, Tile.view map.tileSize tile )) <|
                Map.tiles map
    in
        Html.Keyed.node "div" attrs content


zoomEvents : ZoomLevel -> List (Html.Attribute (Msg msg))
zoomEvents zoom =
    Zoom.events { zoom = Zoom, pinchStart = PinchStart, pinchTo = PinchTo, pinchStop = PinchStop } zoom


dragEvents : Maybe Drag -> List (Html.Attribute (Msg msg))
dragEvents drag =
    Drag.events { dragStart = DragStart, dragTo = DragTo, dragStop = DragStop } drag
