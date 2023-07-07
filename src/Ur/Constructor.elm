module Ur.Constructor exposing
    ( cell, sig, bytes
    , int, signedInt, bigint
    , float32, float64
    , cord, tape
    , listOf
    )

{-| Different ways of constructing `Noun` from Elm types.


# Primitive constructors

@docs cell, sig, bytes


# Integers

@docs int, signedInt, bigint


# Floats

@docs float32, float64


# String

@docs cord, tape


# Higher-order constructors

@docs listOf

-}

import BigInt exposing (BigInt)
import BigInt.Bytes
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as BE
import Ur.Types exposing (..)


{-| Constructs a [Cell](https://developers.urbit.org/reference/glossary/cell) from two Nouns.
-}
cell : Noun -> Noun -> Noun
cell a b =
    Cell ( a, b )


{-| Constructs an `Atom` from an unsigned 32-bit `Int`.
If you pass something other than an unsigned 32-bit integer the behaviour is undefined.
-}
int : Int -> Noun
int i =
    Atom
        (BE.encode
            (if i <= 255 then
                BE.unsignedInt8 i

             else if i <= 65535 then
                BE.unsignedInt16 LE i

             else
                BE.unsignedInt32 LE i
            )
        )


{-| Constructs an `Atom` from a `BigInt`.
-}
bigint : BigInt -> Noun
bigint x =
    Atom (BigInt.Bytes.encode x)


{-| Constructs an `Atom` from a signed `Int`.
-}
signedInt : Int -> Noun
signedInt i =
    int
        (if i < 0 then
            negate (Bitwise.shiftLeftBy 1 i) - 1

         else
            Bitwise.shiftLeftBy 1 i
        )


{-| Constructs an `Atom` from `Bytes`.
-}
bytes : Bytes -> Noun
bytes =
    Atom


{-| Constructs a [`cord`](https://developers.urbit.org/reference/glossary/cord), [`knot`](https://developers.urbit.org/guides/additional/strings#knot) or a [`term`](https://developers.urbit.org/guides/additional/strings#term) from a String.
-}
cord : String -> Noun
cord s =
    Atom (BE.encode (BE.string s))


{-| Constructs a [`tape`](https://developers.urbit.org/reference/glossary/tape) from a `String`.
-}
tape : String -> Noun
tape s =
    String.toList s |> List.map String.fromChar |> listOf cord


{-| Constructs a `sig`-terminated list.

The first argument is a constructor for the elements of the list.

The second argument is the list of values to encode.

-}
listOf : (a -> Noun) -> List a -> Noun
listOf encEl list =
    case list of
        [] ->
            sig

        x :: xs ->
            cell (encEl x) (listOf encEl xs)


{-| The [`sig`](https://developers.urbit.org/reference/glossary/tape) value. Also known as `~`.

It is equivalent to `0`.

-}
sig : Noun
sig =
    Atom (BE.encode (BE.sequence []))


{-| Constructs an `Atom` from a 32-bit float.
-}
float32 : Float -> Noun
float32 f =
    Atom (BE.encode (BE.float32 LE f))


{-| Constructs an `Atom` from a 64-bit float.
-}
float64 : Float -> Noun
float64 f =
    Atom (BE.encode (BE.float64 LE f))
