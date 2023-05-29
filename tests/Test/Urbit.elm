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
                maybeNounEq (Just n) (jam n |> cue)
            )
        , describe "cue"
            [ test "[1 2]"
                (\() ->
                    maybeNounEq
                        (cue (Bytes.fromByteValues [ 0x31, 0x12 ]))
                        (Cell ( Atom (Bytes.fromByteValues [ 1 ]), Atom (Bytes.fromByteValues [ 2 ]) ) |> Just)
                )
            , test "[1 1]"
                (\() ->
                    maybeNounEq
                        (cue (Bytes.fromByteValues [ 0x31, 0x03 ]))
                        (Cell ( Atom (Bytes.fromByteValues [ 1 ]), Atom (Bytes.fromByteValues [ 1 ]) ) |> Just)
                )
            , test "[[1 1] [1 1]]"
                (\() ->
                    maybeNounEq
                        (cue (Bytes.fromByteValues [ 0xC5, 0x3C, 0x09 ]))
                        (let
                            oneOne =
                                Cell
                                    ( Atom (Bytes.fromByteValues [ 1 ])
                                    , Atom (Bytes.fromByteValues [ 1 ])
                                    )
                         in
                         Cell ( oneOne, oneOne ) |> Just
                        )
                )
            , test "[0x1234.5678 0x1234.5678]"
                (\() ->
                    maybeNounEq
                        (cue (Bytes.fromByteValues [ 0x01, 0x1B, 0xCF, 0x8A, 0x46, 0x4E, 0x02 ]))
                        (let
                            bigNum =
                                Atom (Bytes.fromByteValues [ 0x78, 0x56, 0x34, 0x12 ])
                         in
                         Cell ( bigNum, bigNum ) |> Just
                        )
                )
            ]
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
