module Maps.Internal.Bounds
    exposing
        ( Bounds(..)
        , zoom
        , center
        )

{-| This module defines the Bounds type for the Maps library.
The Bounds type is used for defining a geographical area.


# Definition

@docs Bounds


# Properties

@docs zoom
@docs center

-}

import Maps.Internal.LatLng as LatLng exposing (LatLng)
import Maps.Internal.Screen as Screen exposing (ZoomLevel)
import Maps.Internal.Utils exposing (wrap)


{-| The Bounds type defines the bounds of a map.
It can be a rectangular bounding box defined by two points, or a point and a zoom level.
-}
type Bounds
    = Bounds
        { northEast : LatLng
        , southWest : LatLng
        }
    | Centered
        { zoom : Float
        , center : LatLng
        }


{-| The zoom function calculates the zoom level necessary to contain the given Bounds.

Note that the size of the tiles and map are needed to calculate the zoom level.

-}
zoom : Float -> Float -> Float -> Bounds -> ZoomLevel
zoom tileSize mapWidth mapHeight boundsArg =
    case boundsArg of
        Bounds bounds ->
            let
                ( ne, sw ) =
                    ( bounds.northEast, bounds.southWest )

                -- The following assumes a Mercator projection
                -- See https://en.wikipedia.org/wiki/Mercator_projection#Alternative_expressions for details
                latY lat =
                    sin (lat * pi / 180)

                radX2 lat =
                    (logBase e ((1 + latY lat) / (1 - latY lat))) / 2

                latRad lat =
                    (max (-pi) <| min (radX2 lat) pi) / 2

                latFraction =
                    (latRad ne.lat) - (latRad sw.lat)

                lngFraction =
                    ((ne.lng - sw.lng) |> wrap 0 360) / 360
            in
                min
                    (getZoomVal mapWidth tileSize lngFraction)
                    (getZoomVal mapHeight tileSize latFraction)

        Centered boundsValue ->
            boundsValue.zoom


getZoomVal mapSize tileSize frac =
    logBase 2 (mapSize / tileSize / frac)


{-| Calculates the center point of a given Bounds.
-}
center : Bounds -> LatLng
center boundsArg =
    case boundsArg of
        Bounds bounds ->
            { lat = (bounds.northEast.lat + bounds.southWest.lat) / 2
            , lng = (bounds.northEast.lng + bounds.southWest.lng) / 2
            }

        Centered bounds ->
            bounds.center
