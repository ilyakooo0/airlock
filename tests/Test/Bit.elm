module Test.Bit exposing (tests)

import BitParser
import BitWriter
import Bytes
import Expect
import Test exposing (..)
import Test.Utils exposing (..)


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
