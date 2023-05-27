module Test.Urbit exposing (tests)

import BitParser
import BitWriter
import Bytes exposing (Bytes)
import Bytes.Extra as Bytes
import Fuzz exposing (Fuzzer)
import List.Extra as List
import Test exposing (..)
import Test.Utils exposing (..)
import Urbit exposing (..)


tests : Test
tests =
    concat
        [ fuzz atom
            "mat <-> rub"
            (\a ->
                maybeBytesEq (Just a) (BitWriter.run (mat a BitWriter.empty) |> BitParser.run rub)
            )
        ]


atom : Fuzzer Bytes
atom =
    bytes
        |> Fuzz.map
            (\a -> a |> Bytes.toByteValues |> List.dropWhileRight (\x -> x == 0) |> Bytes.fromByteValues)


noun : Fuzzer Noun
noun =
    (\() ->
        Fuzz.oneOf
            [ bytes |> Fuzz.map Atom
            , Fuzz.map2 (\a b -> Cell ( a, b )) noun noun
            ]
    )
        ()
