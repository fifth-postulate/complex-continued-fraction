module RationalTest exposing (fraction, suite)

import Expect
import Fuzz exposing (..)
import Rational as Fraction exposing (Rational)
import Test exposing (..)
import Util exposing (uncurry)


suite : Test
suite =
    describe "Rational Module"
        [ describe "addition"
            [ fuzz fraction "zero is left neutral" <|
                \expected ->
                    let
                        actual =
                            Fraction.add Fraction.zero expected
                    in
                    Expect.equal actual expected
            , fuzz fraction "zero is right neutral" <|
                \expected ->
                    let
                        actual =
                            Fraction.add expected Fraction.zero
                    in
                    Expect.equal actual expected
            , fuzz2 fraction fraction "is commutative" <|
                \p q ->
                    Expect.equal (Fraction.add p q) (Fraction.add q p)
            , fuzz3 fraction fraction fraction "is associative" <|
                \p q r ->
                    let
                        left =
                            Fraction.add p (Fraction.add q r)

                        right =
                            Fraction.add (Fraction.add p q) r
                    in
                    Expect.equal left right
            , fuzz fraction "negate is additive inverse" <|
                \p ->
                    let
                        inverse =
                            Fraction.negate p

                        actual =
                            Fraction.add p inverse
                    in
                    Expect.equal actual Fraction.zero
            ]
        , describe "multiplication"
            [ fuzz fraction "one is left neutral" <|
                \expected ->
                    let
                        actual =
                            Fraction.multiply Fraction.one expected
                    in
                    Expect.equal actual expected
            , fuzz fraction "one is right neutral" <|
                \expected ->
                    let
                        actual =
                            Fraction.multiply expected Fraction.one
                    in
                    Expect.equal actual expected
            , fuzz2 fraction fraction "is commutative" <|
                \p q ->
                    Expect.equal (Fraction.multiply p q) (Fraction.multiply q p)
            , fuzz3 fraction fraction fraction "is associative" <|
                \p q r ->
                    let
                        left =
                            Fraction.multiply p (Fraction.multiply q r)

                        right =
                            Fraction.multiply (Fraction.multiply p q) r
                    in
                    Expect.equal left right
            , fuzz (Fuzz.filter (\f -> not (Fraction.isZero f)) fraction) "invert is multiplicative inverse" <|
                \p ->
                    let
                        inverse =
                            Fraction.invert p
                                |> Maybe.withDefault Fraction.zero

                        actual =
                            Fraction.multiply p inverse
                    in
                    Expect.equal actual Fraction.one
            ]
        ]


fraction : Fuzzer Rational
fraction =
    pair (intRange -100 100) (intRange 1 1000)
        |> Fuzz.map (uncurry Fraction.create)
        |> Fuzz.map (Maybe.withDefault Fraction.zero)
