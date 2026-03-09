module Util exposing (rotate, swap, swivel, uncurry, zip)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


zip : List a -> List b -> List ( a, b )
zip xs ys =
    let
        go : List ( a, b ) -> List a -> List b -> List ( a, b )
        go acc us vs =
            case ( us, vs ) of
                ( u :: uss, v :: vss ) ->
                    go (( u, v ) :: acc) uss vss

                _ ->
                    List.reverse acc
    in
    go [] xs ys


rotate : List a -> List a
rotate =
    swivel 1


swivel : Int -> List a -> List a
swivel n us =
    let
        n_ =
            if n < 0 then
                List.length us + n

            else
                n

        hs =
            List.take n_ us

        ts =
            List.drop n_ us
    in
    List.append ts hs


swap : (b -> a -> c) -> a -> b -> c
swap f a b =
    f b a
