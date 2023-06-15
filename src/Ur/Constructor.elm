module Ur.Constructor exposing
    ( Constructor
    , cell
    , cord
    , float32
    , float64
    , int
    , int64
    , listOf
    , sig
    , signedInt
    , tape
    )

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as BE
import Int64 exposing (Int64)
import Ur exposing (..)


type alias Constructor a =
    a -> Noun


cell : Noun -> Noun -> Noun
cell a b =
    Cell ( a, b )


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


int64 : Int64 -> Noun
int64 i =
    Atom (BE.encode (Int64.encoder LE i))


signedInt : Int -> Noun
signedInt i =
    int
        (if i < 0 then
            negate (Bitwise.shiftLeftBy 1 i) - 1

         else
            Bitwise.shiftLeftBy 1 i
        )


bytes : Bytes -> Noun
bytes =
    Atom


cord : String -> Noun
cord s =
    Atom (BE.encode (BE.string s))


tape : String -> Noun
tape s =
    String.toList s |> List.map String.fromChar |> listOf cord


listOf : (a -> Noun) -> List a -> Noun
listOf encEl list =
    case list of
        [] ->
            sig

        x :: xs ->
            cell (encEl x) (listOf encEl xs)


sig : Noun
sig =
    Atom (BE.encode (BE.sequence []))


float32 : Float -> Noun
float32 f =
    Atom (BE.encode (BE.float32 LE f))


float64 : Float -> Noun
float64 f =
    Atom (BE.encode (BE.float64 LE f))
