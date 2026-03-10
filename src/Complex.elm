module Complex exposing (Complex, add, create, divide, i, invert, isZero, multiply, negate, one, scale, subtract, toString, zero)

import Rational exposing (Rational)


type Complex
    = Cartesian Rational Rational


zero : Complex
zero =
    Cartesian Rational.zero Rational.zero


one : Complex
one =
    Cartesian Rational.one Rational.zero


i : Complex
i =
    Cartesian Rational.zero Rational.one


isZero : Complex -> Bool
isZero f =
    case f of
        Cartesian x y ->
            x == Rational.zero && y == Rational.zero


create : Rational -> Rational -> Complex
create real imaginary =
    Cartesian real imaginary


toString : Complex -> String
toString a =
    case a of
        Cartesian real imaginary ->
            case ( Rational.isZero real, Rational.isZero imaginary ) of
                ( True, True ) ->
                    "0"

                ( False, True ) ->
                    Rational.toString real

                ( True, False ) ->
                    Rational.toString imaginary ++ "i"

                ( False, False ) ->
                    Rational.toString real ++ "+" ++ Rational.toString imaginary ++ "i"


add : Complex -> Complex -> Complex
add a b =
    case ( a, b ) of
        ( Cartesian x y, Cartesian u v ) ->
            Cartesian (Rational.add x u) (Rational.add y v)


multiply : Complex -> Complex -> Complex
multiply a b =
    case ( a, b ) of
        ( Cartesian x y, Cartesian u v ) ->
            Cartesian
                (Rational.subtract
                    (Rational.multiply x u)
                    (Rational.multiply y v)
                )
                (Rational.add
                    (Rational.multiply x v)
                    (Rational.multiply y u)
                )


negate : Complex -> Complex
negate a =
    case a of
        Cartesian x y ->
            Cartesian (Rational.negate x) (Rational.negate y)


invert : Complex -> Maybe Complex
invert a =
    case a of
        Cartesian real imaginary ->
            let
                d =
                    Rational.add
                        (Rational.multiply real real)
                        (Rational.multiply imaginary imaginary)
            in
            if not (Rational.isZero d) then
                Cartesian
                    (Rational.divide real d |> Maybe.withDefault Rational.zero)
                    (Rational.divide imaginary d |> Maybe.withDefault Rational.zero |> Rational.negate)
                    |> Just

            else
                Nothing


subtract : Complex -> Complex -> Complex
subtract a b =
    add a (negate b)


divide : Complex -> Complex -> Maybe Complex
divide a b =
    b
        |> invert
        |> Maybe.map (multiply a)


scale : Rational -> Complex -> Complex
scale t a =
    case a of
        Cartesian x y ->
            Cartesian (Rational.multiply t x) (Rational.multiply t y)
