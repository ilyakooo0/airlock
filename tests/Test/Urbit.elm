module Test.Urbit exposing (tests)

import BigInt exposing (BigInt)
import BigInt.Bytes
import BitParser
import BitWriter
import Bytes exposing (Bytes)
import Bytes.Extra as Bytes
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import List.Extra as List
import Test exposing (..)
import Test.Utils exposing (..)
import Ur exposing (..)
import Ur.Constructor as C
import Ur.Deconstructor as D
import Ur.Jam exposing (cue, jam, mat, rub)
import Ur.Types exposing (..)
import Ur.Uw


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
        , describe "Decon"
            [ test "[1 2]"
                (\() ->
                    Expect.equal
                        (Just ( 1, 2 ))
                        (D.runBytes
                            (D.cell D.int D.int)
                            (Bytes.fromByteValues [ 0x31, 0x12 ])
                        )
                )
            , test "1.686.761.906.334"
                (\() ->
                    Expect.equal
                        (Just "1686761906334")
                        (D.runBytes
                            D.bigint
                            (Bytes.fromByteValues [ 0x80, 0xC9, 0x13, 0x04, 0x5B, 0x17, 0x31 ])
                            |> Maybe.map BigInt.toString
                        )
                )
            , test "[4 ~[1 2 3]]"
                (\() ->
                    Expect.equal
                        (Just ( 4, [ 1, 2, 3 ] ))
                        (D.runBytes
                            (D.cell D.int (D.list D.int))
                            (Bytes.fromByteValues [ 0x61, 0xC6, 0x21, 0x43, 0x0B ])
                        )
                )
            , test "[.8 .11]"
                (\() ->
                    Expect.equal
                        (Just ( 8, 11 ))
                        (D.runBytes
                            (D.cell D.float32 D.float32)
                            (Bytes.fromByteValues [ 0x01, 0x1F, 0x00, 0x00, 0x20, 0x08, 0x7C, 0x00, 0x00, 0x98, 0x20 ])
                        )
                )
            , test "[8 'hi']"
                (\() ->
                    Expect.equal
                        (Just ( 8, "hi" ))
                        (D.runBytes
                            (D.cell D.int D.cord)
                            (Bytes.fromByteValues [ 0x41, 0x10, 0x3C, 0x5A, 0x1A ])
                        )
                )
            , test "[8 \"hi\"]"
                (\() ->
                    Expect.equal
                        (Just ( 8, "hi" ))
                        (D.runBytes
                            (D.cell D.int D.tape)
                            (Bytes.fromByteValues [ 0x41, 0x30, 0x38, 0x3A, 0x78, 0x5A ])
                        )
                )
            , describe "sum types"
                [ test "[%tape \"hi\"]"
                    (\() ->
                        Expect.equal
                            (Just "hi")
                            (D.runBytes
                                (D.oneOf
                                    [ D.cell (D.const D.cord "tape") D.tape |> D.map (\((), t) -> t)
                                    , D.cell (D.const D.cord "cord") D.cord |> D.map (\((), c) -> c)
                                    ]
                                )
                                (Bytes.fromByteValues [ 0x01, 0x9F, 0x2E, 0x0C, 0xAE, 0x1C, 0x1C, 0x1D, 0x3C, 0x2D ])
                            )
                    )
                , test "[%cord 'hi']"
                    (\() ->
                        Expect.equal
                            (Just "hi")
                            (D.runBytes
                                (D.oneOf
                                    [ D.cell (D.const D.cord "tape") D.tape |> D.map (\((), t) -> t)
                                    , D.cell (D.const D.cord "cord") D.cord |> D.map (\((), c) -> c)
                                    ]
                                )
                                (Bytes.fromByteValues [ 0x01, 0x7F, 0xEC, 0x4D, 0x8E, 0x0C, 0x1E, 0x2D, 0x0D ])
                            )
                    )
                ]
            , test "[--8 -8]"
                (\() ->
                    Expect.equal
                        (Just ( 8, -8 ))
                        (D.runBytes
                            (D.cell D.signedInt D.signedInt)
                            (Bytes.fromByteValues [ 0xC1, 0x20, 0xE4, 0x01 ])
                        )
                )
            , test "Int 65.600"
                (\() ->
                    Expect.equal
                        (Just 65600)
                        (D.runBytes
                            D.int
                            (Bytes.fromByteValues [ 0xC0, 0x00, 0x02, 0x08 ])
                        )
                )
            , test "BigInt 65.600"
                (\() ->
                    Expect.equal
                        (Just (BigInt.fromInt 65600))
                        (D.runBytes
                            D.bigint
                            (Bytes.fromByteValues [ 0xC0, 0x00, 0x02, 0x08 ])
                        )
                )
            ]
        , describe "Constructor <-> Deconstructor"
            [ Test.fuzz
                (Fuzz.triple Fuzz.niceFloat Fuzz.int (Fuzz.intAtLeast 0))
                "[@rd @s @u]"
                (\( f, i, ui ) ->
                    Expect.equal (Just ( f, i, ui ))
                        (D.run
                            (D.cell D.float64 (D.cell D.signedInt D.int) |> D.map (\(a, (b, c)) -> ( a, b, c )))
                            (C.cell (C.float64 f) (C.cell (C.signedInt i) (C.int ui)))
                        )
                )
            , Test.fuzz
                (Fuzz.list Fuzz.int)
                "~[@s]"
                (\x ->
                    Expect.equal
                        (Just x)
                        (D.run
                            (D.list D.signedInt)
                            (C.listOf C.signedInt x)
                        )
                )
            , Test.fuzz
                Fuzz.string
                "cord"
                (\x ->
                    Expect.equal
                        (Just x)
                        (D.run
                            D.cord
                            (C.cord x)
                        )
                )
            , Test.fuzz
                Fuzz.string
                "tape"
                (\x ->
                    Expect.equal
                        (Just x)
                        (D.run
                            D.tape
                            (C.tape x)
                        )
                )
            , Test.fuzz bigint "bigint" (\x -> Expect.equal (Just x) (D.run D.bigint (C.bigint x)))
            ]
        , Test.describe "Ur.Uw"
            [ Test.fuzz atom
                "encode <-> decode"
                (\bs -> bytesEq bs (bs |> Ur.Uw.encode |> Ur.Uw.decode |> stripTrailingZeros))
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
            stripTrailingZeros


stripTrailingZeros : Bytes -> Bytes
stripTrailingZeros =
    Bytes.toByteValues >> List.dropWhileRight (\x -> x == 0) >> Bytes.fromByteValues


noun : () -> Fuzzer Noun
noun () =
    Fuzz.frequency
        [ ( 0.6, atom |> Fuzz.map Atom )
        , ( 0.4, Fuzz.map2 (\a b -> Cell ( a, b )) (Fuzz.lazy noun) (Fuzz.lazy noun) )
        ]


bigint : Fuzzer BigInt
bigint =
    bytes |> Fuzz.map BigInt.Bytes.decode
