module Maps.Internal.Utils
    exposing
        ( wrap
        , sinh
        , cartesianMap
        , noDefaultButPropagate
        , flip
        )


wrap min max n =
    if n < min then
        wrap min max <| n + (max - min)
    else if n >= max then
        wrap min max <| n - (max - min)
    else
        n


sinh x =
    ((e ^ x) - (e ^ -x)) / 2


cartesianMap : (a -> b -> c) -> List a -> List b -> List (List c)
cartesianMap function rows columns =
    (flip function)
        |> (flip List.map) columns
        |> List.map ((flip List.map) rows)


flip f a b =
    f b a


noDefaultButPropagate msg =
    { message = msg, preventDefault = True, stopPropagation = False }
