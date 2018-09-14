module Maps.Internal.Tile
    exposing
        ( Tile
        , Offset
        , makeUrl
        , fromLatLng
        , toLatLng
        , view
        )

import Regex exposing (fromString, never)
import Html exposing (Html)
import Html.Attributes as Attr
import Maps.Internal.LatLng as LatLng exposing (LatLng)
import Maps.Internal.Utils exposing (wrap, sinh)


fromFloat =
    String.fromFloat


type alias Url =
    String


type alias Tile =
    ( Url, Offset )


type alias Offset =
    { x : Float
    , y : Float
    }


makeUrl : String -> Int -> Int -> Int -> Url
makeUrl tileServer zoom x y =
    tileServer
        |> formatInt "{z}" zoom
        |> formatInt "{x}" x
        |> formatInt "{y}" y


fromLatLng : Float -> LatLng -> Offset
fromLatLng zoom loc =
    let
        n =
            2 ^ zoom

        x =
            n * ((loc.lng + 180) / 360) |> wrap 0 n

        latRad =
            loc.lat * pi / 180

        y =
            n * (1 - (logBase e <| abs <| tan latRad + (1 / cos latRad)) / pi) / 2
    in
        Offset x y


toLatLng : Float -> Float -> Float -> LatLng
toLatLng zoom tileX tileY =
    let
        n =
            2 ^ zoom

        lngDeg =
            tileX / n * 360 - 180 |> wrap -180 180

        latRad =
            atan <| sinh <| pi * (1 - 2 * tileY / n)

        latDeg =
            latRad * 180 / pi
    in
        LatLng latDeg lngDeg


formatInt : String -> Int -> String -> String
formatInt replace number =
    Regex.replace (fromString replace |> Maybe.withDefault never) (\_ -> String.fromInt number)


view : Float -> Tile -> Html msg
view tileSize ( url, offset ) =
    Html.img
        [ Attr.src url
        , Attr.style "position" "absolute"
        , Attr.style "left" (fromFloat offset.x ++ "px")
        , Attr.style "top" (fromFloat offset.y ++ "px")
        , Attr.style "width" (fromFloat tileSize ++ "px")
        , Attr.style "height" (fromFloat tileSize ++ "px")
        , Attr.style "background-color" "rgba(0,0,0, 0)"
        ]
        []
