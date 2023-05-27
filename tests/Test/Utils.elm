module Test.Utils exposing (..)

import Bytes exposing (Bytes)
import Bytes.Encode as BE
import Bytes.Extra
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)


bytes : Fuzzer Bytes
bytes =
    (Fuzz.intRange 0 255 |> Fuzz.list)
        |> Fuzz.map (\l -> l |> List.map BE.unsignedInt8 |> BE.sequence |> BE.encode)


bytesEq : Bytes -> Bytes -> Expectation
bytesEq a b =
    Expect.equal (Bytes.Extra.toByteValues a) (Bytes.Extra.toByteValues b)


maybeBytesEq : Maybe Bytes -> Maybe Bytes -> Expectation
maybeBytesEq a b =
    Expect.equal (Maybe.map Bytes.Extra.toByteValues a) (Maybe.map Bytes.Extra.toByteValues b)
