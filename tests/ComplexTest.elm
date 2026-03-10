module ComplexTest exposing (complex, suite)

import Complex exposing (Complex)
import Expect
import Fuzz exposing (..)
import Rational
import RationalTest exposing (fraction)
import Test exposing (..)


suite : Test
suite =
    describe "Complex Module"
        [ describe "addition"
            [ fuzz complex "zero is left neutral" <|
                \expected ->
                    let
                        actual =
                            Complex.add Complex.zero expected
                    in
                    Expect.equal actual expected
            , fuzz complex "zero is right neutral" <|
                \expected ->
                    let
                        actual =
                            Complex.add expected Complex.zero
                    in
                    Expect.equal actual expected
            , fuzz2 complex complex "is commutative" <|
                \p q ->
                    Expect.equal (Complex.add p q) (Complex.add q p)
            , fuzz3 complex complex complex "is associative" <|
                \p q r ->
                    let
                        left =
                            Complex.add p (Complex.add q r)

                        right =
                            Complex.add (Complex.add p q) r
                    in
                    Expect.equal left right
            , fuzz complex "negate is additive inverse" <|
                \p ->
                    let
                        inverse =
                            Complex.negate p

                        actual =
                            Complex.add p inverse
                    in
                    Expect.equal actual Complex.zero
            ]
        , describe "multiplication"
            [ fuzz complex "one is left neutral" <|
                \expected ->
                    let
                        actual =
                            Complex.multiply Complex.one expected
                    in
                    Expect.equal actual expected
            , fuzz complex "one is right neutral" <|
                \expected ->
                    let
                        actual =
                            Complex.multiply expected Complex.one
                    in
                    Expect.equal actual expected
            , fuzz2 complex complex "is commutative" <|
                \p q ->
                    Expect.equal (Complex.multiply p q) (Complex.multiply q p)
            , fuzz3 complex complex complex "is associative" <|
                \p q r ->
                    let
                        left =
                            Complex.multiply p (Complex.multiply q r)

                        right =
                            Complex.multiply (Complex.multiply p q) r
                    in
                    Expect.equal left right
            , test "particular asscociative test" <|
                \_ ->
                    let
                        p =
                            Complex.create
                                Rational.one
                                (231 |> Rational.fromInt |> Rational.invert |> Maybe.withDefault Rational.one)

                        q =
                            Complex.create
                                (23 |> Rational.fromInt |> Rational.invert |> Maybe.withDefault Rational.one)
                                (791 |> Rational.fromInt |> Rational.invert |> Maybe.withDefault Rational.one)

                        r =
                            Complex.create
                                Rational.zero
                                (511 |> Rational.fromInt |> Rational.invert |> Maybe.withDefault Rational.one)

                        left =
                            Complex.multiply p (Complex.multiply q r)

                        right =
                            Complex.multiply (Complex.multiply p q) r
                    in
                    Expect.equal left right

            -- , fuzz (Fuzz.filter (\f -> not (Complex.isZero f)) complex) "invert is multiplicative inverse" <|
            --     \p ->
            --         let
            --             inverse =
            --                 Complex.invert p
            --                     |> Maybe.withDefault Complex.zero
            --             actual =
            --                 Complex.multiply p inverse
            --         in
            --         Expect.equal actual Complex.one
            ]
        ]


complex : Fuzzer Complex
complex =
    Fuzz.map2 Complex.create fraction fraction
