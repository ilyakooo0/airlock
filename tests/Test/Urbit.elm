module Test.Urbit exposing (tests)

import BitParser
import BitWriter
import Bytes exposing (Bytes)
import Bytes.Extra as Bytes
import Expect exposing (Expectation)
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
        , fuzz (noun ())
            "jam <-> cue"
            (\n ->
                maybeNounEq (Just n) (BitWriter.run (jam n BitWriter.empty) |> BitParser.run cue)
            )
        ]


maybeNounEq : Maybe Noun -> Maybe Noun -> Expectation
maybeNounEq a b =
    case ( a, b ) of
        ( Just x, Just y ) ->
            nounEq x y

        _ ->
            Expect.equal a b


nounEq : Noun -> Noun -> Expectation
nounEq a b =
    case ( a, b ) of
        ( Atom x, Atom y ) ->
            bytesEq x y

        ( Cell ( x1, y1 ), Cell ( x2, y2 ) ) ->
            Expect.all [ nounEq x1 x2 |> always, nounEq y1 y2 |> always ] ()

        _ ->
            Expect.fail (Debug.toString a ++ " /= " ++ Debug.toString b)


atom : Fuzzer Bytes
atom =
    bytes
        |> Fuzz.map
            (\a -> a |> Bytes.toByteValues |> List.dropWhileRight (\x -> x == 0) |> Bytes.fromByteValues)


noun : () -> Fuzzer Noun
noun () =
    Fuzz.frequency
        [ ( 0.6, atom |> Fuzz.map Atom )
        , ( 0.4, Fuzz.map2 (\a b -> Cell ( a, b )) (Fuzz.lazy noun) (Fuzz.lazy noun) )
        ]
