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

The principal (and types) are very similar to `Url.Parser` from `elm/url`.

You would parse a `[%edit @ cord]` like this:

     type alias Edit = {id: Int, text : String}

     (D.cell (D.const D.cord "edit") (D.cell D.int D.cord)) |> D.map Edit

When you `map` a `Deconstructor` the function you pass to `map` will accept exactly the number of arguments
that "captured" values in exacly the order they occured in the `Deconstructor`.

In our case we `map` the `Edit` type, which accepts exaclty two arguments because there are two `Deconstructor`s
that "capture" a value: `D.int` and `D.cord`.

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
type Deconstructor a b
    = Deconstructor (Noun -> a -> Maybe b)


{-| Executes a `Deconstructor` on a `Noun`.
-}
run : Deconstructor (a -> a) a -> Noun -> Maybe a
run (Deconstructor f) noun =
    f noun identity


{-| Executes a deconstructor on a `jam`med `Noun`.
-}
runBytes : Deconstructor (a -> a) a -> Bytes -> Maybe a
runBytes (Deconstructor f) bs =
    cue bs
        |> Maybe.andThen
            (\noun -> f noun identity)


{-| Asserts that the value at the current position should be exactly equal to the second argument.

The first argument is a `Deconstructor` for the gven tyoe.

The second argument is the value to compare with.

This is useful to match on `term`s when there are multiple possible cases in a head-tagged union.

-}
const : Deconstructor (a -> a) a -> a -> Deconstructor c c
const (Deconstructor f) value =
    Deconstructor
        (\noun c ->
            case f noun identity of
                Just a ->
                    if a == value then
                        Just c

                    else
                        Nothing

                Nothing ->
                    Nothing
        )


{-| Extracts a `cord` at the current location.
-}
cord : Deconstructor (String -> a) a
cord =
    Deconstructor
        (\x f ->
            case x of
                Atom bs ->
                    BD.decode (BD.string (Bytes.width bs)) bs
                        |> Maybe.map f

                Cell _ ->
                    Nothing
        )


{-| Extracts a `tape` at the current location.
-}
tape : Deconstructor (String -> a) a
tape =
    list cord |> fmap String.concat


{-| Extracts a 32-bit unsigned `Int` at the given location.

If the `Atom` at the current location is larger than 32 bits the the behaviour is undefined.

-}
int : Deconstructor (Int -> a) a
int =
    Deconstructor
        (\x f ->
            case x of
                Atom bs ->
                    (case Bytes.width bs of
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
                    )
                        |> Maybe.map f

                Cell _ ->
                    Nothing
        )


{-| Extracts a `BigInt` at the current location.
-}
bigint : Deconstructor (BigInt -> a) a
bigint =
    Deconstructor
        (\x f ->
            case x of
                Atom bs ->
                    BigInt.Bytes.decode bs |> f |> Just

                Cell _ ->
                    Nothing
        )


{-| Extracts a 32-bit signed `Int` at the given location.

If the `Atom` at the current location is larger than 32 bits the the behaviour is undefined.

-}
signedInt : Deconstructor (Int -> a) a
signedInt =
    int
        |> fmap
            (\i ->
                if Bitwise.and 1 i == 1 then
                    -(Bitwise.shiftRightBy 1 (1 + i))

                else
                    Bitwise.shiftRightBy 1 i
            )


{-| Extracts a 32-bit `Float` at the given location.
-}
float32 : Deconstructor (Float -> a) a
float32 =
    Deconstructor
        (\x f ->
            case x of
                Atom bs ->
                    BD.decode (BD.float32 LE) bs
                        |> Maybe.map f

                Cell _ ->
                    Nothing
        )


{-| Extracts a 64-bit `Float` at the given location.
-}
float64 : Deconstructor (Float -> a) a
float64 =
    Deconstructor
        (\x f ->
            case x of
                Atom bs ->
                    BD.decode (BD.float64 LE) bs
                        |> Maybe.map f

                Cell _ ->
                    Nothing
        )


{-| Extracts the raw `Byte`s of an `Atom` at the current location.
-}
bytes : Deconstructor (Bytes -> a) a
bytes =
    Deconstructor
        (\n f ->
            case n of
                Atom bs ->
                    f bs |> Just

                Cell _ ->
                    Nothing
        )


{-| Asserts the the `Atom` at the current position should be exactly `sig` (`~`).
-}
sig : Deconstructor a a
sig =
    Deconstructor
        (\n a ->
            case n of
                Atom b ->
                    if (Bytes.Extra.toByteValues b |> List.filter (\x -> x /= 0)) == [] then
                        Just a

                    else
                        Nothing

                Cell _ ->
                    Nothing
        )


{-| Extracts a sig-terminated list of arbitrary elements at the current position.

The first argument is a `Deconstructor` of the elements of the list.

-}
list : Deconstructor (a -> a) a -> Deconstructor (List a -> b) b
list (Deconstructor f) =
    Deconstructor
        (\n g ->
            let
                go n_ =
                    case n_ of
                        Atom _ ->
                            Just []

                        Cell ( el, tail ) ->
                            Maybe.map2 (\a b -> a :: b) (f el identity) (go tail)
            in
            go n |> Maybe.map g
        )


alt : Deconstructor a b -> Deconstructor a b -> Deconstructor a b
alt (Deconstructor f) (Deconstructor g) =
    Deconstructor
        (\n a ->
            case f n a of
                Just x ->
                    Just x

                Nothing ->
                    g n a
        )


{-| Try to execute all of the `Deconstructor`s in order until one succeeds.

This is especially useful for deconstructing head-tagged unions from Hoon.

-}
oneOf : List (Deconstructor a b) -> Deconstructor a b
oneOf l =
    case l of
        [] ->
            Deconstructor (\_ _ -> Nothing)

        x :: xs ->
            alt x (oneOf xs)


{-| Extract the raw `Noun` at the current position.

Always succeeds.

-}
tar : Deconstructor (Noun -> a) a
tar =
    Deconstructor (\noun f -> Just (f noun))


{-| Ignore any value at the current position.

This is useful when you have a value you don't care about. `ignore` allows you to just skip the value.

-}
ignore : Deconstructor a a
ignore =
    Deconstructor (\_ f -> Just f)


{-| Extracts a [`Cell`](https://developers.urbit.org/reference/glossary/cell) (pair) of two arbitrary values.

Accepts two arbitrary `Deconstructor`s that form a Cell.

-}
cell : Deconstructor a b -> Deconstructor b c -> Deconstructor a c
cell (Deconstructor l) (Deconstructor r) =
    Deconstructor
        (\noun a ->
            case noun of
                Cell ( lhs, rhs ) ->
                    l lhs a |> Maybe.andThen (\b -> r rhs b)

                Atom _ ->
                    Nothing
        )


{-| -}
lazy : (() -> Deconstructor a b) -> Deconstructor a b
lazy f =
    Deconstructor
        (\noun a ->
            case f () of
                Deconstructor g ->
                    g noun a
        )


{-| Maps (applies) a function to all of the values deconstructed.

This is useful when you want to create a data type with extracted values as fields.

-}
map : a -> Deconstructor a b -> Deconstructor (b -> c) c
map a (Deconstructor f) =
    Deconstructor (\noun g -> f noun a |> Maybe.map g)


fmap : (a -> b) -> Deconstructor (a -> c) c -> Deconstructor (b -> c) c
fmap f (Deconstructor g) =
    Deconstructor (\noun h -> g noun (f >> h))
