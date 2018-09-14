module Maps.Convert exposing
    ( MapSizes
    , screenOffsetToLatLng
    , latLngToScreenOffset
    , getMap
    )

{-| Functions for converting between different map units.


# Map Size Properties

@docs MapSizes


# Screen Offset - Latitude/Longitude

@docs screenOffsetToLatLng
@docs latLngToScreenOffset

-}

import Maps.Geoss
import Maps.Internal.OpaqueTypes exposing (Model, getMapInternal)
import Maps.Internal.Screen as Screen exposing (ZoomLevel)



--import Maps.Internal.Maps exposing (Model)


{-| The size properties of a map.
The conversion functions that require this type can just be passed a map type.
-}
type alias MapSizes a =
    { a | tileSize : Float, zoom : ZoomLevel, width : Float, height : Float, center : Maps.Geo.LatLng }


{-| Take an offset from the top left of the map and convert it to a latitude/longitude.
Note that it requires the dimensions of the map (or the map itself) to calculate this conversion.
-}
screenOffsetToLatLng : MapSizes a -> Screen.Offset -> Maps.Geo.LatLng
screenOffsetToLatLng map offset =
    Screen.offsetToLatLng map offset


{-| Take latitude/longitude anc convert it to an offset from the top left of the map.
Note that it requires the dimensions of the map (or the map itself) to calculate this conversion.
-}
latLngToScreenOffset : MapSizes a -> Maps.Geo.LatLng -> Screen.Offset
latLngToScreenOffset map latLng =
    Screen.offsetFromLatLng map latLng


{-| Gets internal map as MapSizes a to use in offset functions
-}
getMap model =
    getMapInternal model
