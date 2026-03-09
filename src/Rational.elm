module Rational exposing (Rational, add, create, divide, floor, invert, isZero, multiply, negate, one, scale, subtract, toFloat, toString, zero)


type Rational
    = Fraction Int Int


zero : Rational
zero =
    Fraction 0 1


one : Rational
one =
    Fraction 1 1


isZero : Rational -> Bool
isZero f =
    case f of
        Fraction numerator _ ->
            numerator == 0


create : Int -> Int -> Maybe Rational
create numerator denominator =
    if denominator == 0 then
        Nothing

    else
        Just (create_ numerator denominator)


create_ : Int -> Int -> Rational
create_ numerator denominator =
    let
        t =
            abs numerator

        n =
            abs denominator

        s =
            sign numerator * sign denominator

        ( g, _, _ ) =
            egcd t n
    in
    Fraction (s * t // g) (n // g)


toString : Rational -> String
toString a =
    case a of
        Fraction n d ->
            String.fromInt n ++ "/" ++ String.fromInt d


sign : Int -> Int
sign n =
    case compare n 0 of
        LT ->
            -1

        EQ ->
            0

        GT ->
            1


egcd : Int -> Int -> ( Int, Int, Int )
egcd a b =
    let
        -- invariant: x = u * a + v * b && y = s * a + t * b
        go : Int -> Int -> Int -> Int -> Int -> Int -> ( Int, Int, Int )
        go x y u v s t =
            if y == 0 then
                ( x, u, v )

            else
                let
                    q =
                        x // y
                in
                go y (x - q * y) s t (u - q * s) (v - q * t)
    in
    go (abs a) (abs b) (sign a) 0 0 (sign b)


add : Rational -> Rational -> Rational
add a b =
    case ( a, b ) of
        ( Fraction na da, Fraction nb db ) ->
            create_ (db * na + da * nb) (da * db)


multiply : Rational -> Rational -> Rational
multiply a b =
    case ( a, b ) of
        ( Fraction na da, Fraction nb db ) ->
            create_ (na * nb) (da * db)


negate : Rational -> Rational
negate a =
    case a of
        Fraction n d ->
            Fraction (Basics.negate n) d


invert : Rational -> Maybe Rational
invert a =
    case a of
        Fraction n d ->
            if n /= 0 then
                Just <| Fraction d n

            else
                Nothing


subtract : Rational -> Rational -> Rational
subtract a b =
    add a (negate b)


divide : Rational -> Rational -> Maybe Rational
divide a b =
    b
        |> invert
        |> Maybe.map (multiply a)


floor : Rational -> Int
floor a =
    case a of
        Fraction n d ->
            n // d


scale : Int -> Rational -> Rational
scale t v =
    case v of
        Fraction n d ->
            create_ (t * n) d


toFloat : Rational -> Float
toFloat v =
    case v of
        Fraction n d ->
            Basics.toFloat n / Basics.toFloat d
