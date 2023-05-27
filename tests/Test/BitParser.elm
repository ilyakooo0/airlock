module Test.BitParser exposing (tests)

import BitParser
import BitWriter
import Bytes exposing (Bytes)
import Bytes.Encode as BE
import Bytes.Extra
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)


tests : Test
tests =
    concat
        [ describe "BitParser"
            [ fuzz bytes
                "parsed bytes are identical"
                (\b ->
                    maybeBytesEq (Just b) (BitParser.run (BitParser.bits (Bytes.width b * 8)) b)
                )
            ]
        , describe "BitWriter"
            [ fuzz bytes
                "printed bytes are identical"
                (\b ->
                    let
                        bits =
                            BitParser.run (BitParser.rawBits (Bytes.width b * 8)) b
                    in
                    case bits of
                        Nothing ->
                            Expect.fail "unexepcted"

                        Just bits_ ->
                            maybeBytesEq (Just b) (BitWriter.empty |> BitWriter.bits bits_ |> BitWriter.run |> Just)
                )
            ]
        ]


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
