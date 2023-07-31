module Ur.Deconstructor exposing
    ( Deconstructor
    , run, runBytes
    , cell, list, oneOf, const
    , map
    , int, signedInt, bigint
    , float32, float64
    , cord, tape
    , bytes, sig, ignore, tar, lazy
    )

{-| This module provides an API to deconstruct `Noun`s into arbitrary Elm data structures.

You would parse a `[%edit @ cord]` like this:

     type alias Edit = {id: Int, text : String}

     (D.cell (D.const D.cord "edit") (D.cell D.bigint D.cord)) |> D.map (\( (), ( id, txt ) ) -> Edit id txt)

@docs Deconstructor


# Running a `Deconstructor`

@docs run, runBytes


# Higher-order `Deconstructor`s

@docs cell, list, oneOf, const


# Mapping a `Deconstructor`

@docs map


# Numbers


## `Int`a

@docs int, signedInt, bigint


## `Float`s

@docs float32, float64


# `String`s

@docs cord, tape


# Miscellaneous

@docs bytes, sig, ignore, tar, lazy

-}

import BigInt exposing (BigInt)
import BigInt.Bytes
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as BD
import Bytes.Encode as BE
import Bytes.Extra
import Ur.Jam exposing (cue)
import Ur.Types exposing (..)


{-| A `Noun` deconstructor.
-}
type alias Deconstructor a =
    Noun -> Maybe a


{-| Executes a `Deconstructor` on a `Noun`.
-}
run : Deconstructor a -> Noun -> Maybe a
run f noun =
    f noun


{-| Executes a deconstructor on a `jam`med `Noun`.
-}
runBytes : Deconstructor a -> Bytes -> Maybe a
runBytes f bs =
    cue bs |> Maybe.andThen f


{-| Asserts that the value should be exactly equal to the second argument.

The first argument is a `Deconstructor` for the given type.

The second argument is the value to compare with.

This is useful to match on `term`s when there are multiple possible cases in a head-tagged union.

-}
const : Deconstructor a -> a -> Deconstructor ()
const f value noun =
    case f noun of
        Just a ->
            if a == value then
                Just ()

            else
                Nothing

        Nothing ->
            Nothing


{-| Extracts a `BigInt`.
-}
bigint : Deconstructor BigInt
bigint x =
    case x of
        Atom bs ->
            BigInt.Bytes.decode bs |> Just

        Cell _ ->
            Nothing


{-| Extracts a 32-bit signed `Int`.

If the `Atom` is larger than 32 bits the the behaviour is undefined.

-}
signedInt : Deconstructor Int
signedInt =
    int
        |> map
            (\i ->
                if Bitwise.and 1 i == 1 then
                    -(Bitwise.shiftRightBy 1 (1 + i))

                else
                    Bitwise.shiftRightBy 1 i
            )


{-| Extracts a 32-bit `Float`.
-}
float32 : Deconstructor Float
float32 x =
    case x of
        Atom bs ->
            BD.decode (BD.float32 LE) bs

        Cell _ ->
            Nothing


{-| Extracts a 64-bit `Float`.
-}
float64 : Deconstructor Float
float64 x =
    case x of
        Atom bs ->
            BD.decode (BD.float64 LE) bs

        Cell _ ->
            Nothing


{-| Extracts the raw `Byte`s of an `Atom`.
-}
bytes : Deconstructor Bytes
bytes n =
    case n of
        Atom bs ->
            Just bs

        Cell _ ->
            Nothing


{-| Asserts the the `Atom` should be exactly `sig` (`~`).
-}
sig : Deconstructor ()
sig n =
    case n of
        Atom b ->
            if (Bytes.Extra.toByteValues b |> List.filter (\x -> x /= 0)) == [] then
                Just ()

            else
                Nothing

        Cell _ ->
            Nothing


{-| Extracts a sig-terminated list of arbitrary elements.

The first argument is a `Deconstructor` of the elements of the list.

-}
list : Deconstructor a -> Deconstructor (List a)
list f n =
    let
        go n_ =
            case n_ of
                Atom _ ->
                    Just []

                Cell ( el, tail ) ->
                    Maybe.map2 (\a b -> a :: b) (f el) (go tail)
    in
    go n


alt : Deconstructor a -> Deconstructor a -> Deconstructor a
alt f g n =
    case f n of
        Just x ->
            Just x

        Nothing ->
            g n


{-| Try to execute all of the `Deconstructor`s in order until one succeeds.

This is especially useful for deconstructing head-tagged unions from Hoon.

-}
oneOf : List (Deconstructor a) -> Deconstructor a
oneOf l =
    case l of
        [] ->
            always Nothing

        x :: xs ->
            alt x (oneOf xs)


{-| Ignore any value.

This is useful when you have a value you don't care about. `ignore` allows you to just skip the value.

-}
ignore : Deconstructor ()
ignore =
    always (Just ())


{-| Extracts a [`Cell`](https://developers.urbit.org/reference/glossary/cell) (pair) of two arbitrary values.

Accepts two arbitrary `Deconstructor`s that form a Cell.

-}
cell : Deconstructor a -> Deconstructor b -> Deconstructor ( a, b )
cell l r noun =
    case noun of
        Cell ( lhs, rhs ) ->
            l lhs |> Maybe.andThen (\a -> r rhs |> Maybe.map (\b -> ( a, b )))

        Atom _ ->
            Nothing


{-| Maps (applies) a function to all of the values deconstructed.

This is useful when you want to create a data type with extracted values as fields.

-}
lazy : (() -> Deconstructor a) -> Deconstructor a
lazy f noun =
    f () noun


{-| Maps over the result of the deconstructor.
-}
map : (a -> b) -> Deconstructor a -> Deconstructor b
map g f noun =
    f noun |> Maybe.map g


{-| Extracts a `cord`.
-}
cord : Deconstructor String
cord x =
    case x of
        Atom bs ->
            BD.decode (BD.string (Bytes.width bs)) bs

        Cell _ ->
            Nothing


{-| Extracts a `tape`.
-}
tape : Deconstructor String
tape =
    list cord |> map String.concat


{-| Extracts a 32-bit unsigned `Int`.

If the `Atom` is larger than 32 bits the the behaviour is undefined.

-}
int : Deconstructor Int
int x =
    case x of
        Atom bs ->
            case Bytes.width bs of
                1 ->
                    BD.decode BD.unsignedInt8 bs

                2 ->
                    BD.decode (BD.unsignedInt16 LE) bs

                3 ->
                    BD.decode (BD.unsignedInt32 LE) (BE.encode (BE.sequence [ BE.bytes bs, BE.unsignedInt8 0 ]))

                4 ->
                    BD.decode (BD.unsignedInt32 LE) bs

                _ ->
                    Nothing

        Cell _ ->
            Nothing


{-| Extract the raw `Noun`.

Always succeeds.

-}
tar : Deconstructor Noun
tar =
    Just
