module Ur.Uw exposing (decode, encode)

{-| This module works with Urbit base-64 encoded strings aka `@uw`.

@docs decode, encode

-}

import BitParser as BP
import BitWriter as BW
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Extra
import Dict
import List.Extra


{-| -}
decode : String -> Bytes
decode string =
    if string == "0w0" then
        Bytes.Extra.empty

    else
        let
            chars =
                string
                    |> String.toList
                    -- 0w
                    |> List.drop 2

            go : List Char -> BW.BitWriter -> BW.BitWriter
            go cs writer =
                case cs of
                    [] ->
                        writer

                    '.' :: rest ->
                        go rest writer

                    c :: rest ->
                        case Dict.get c charToBits of
                            Just bits ->
                                go rest writer |> BW.bits bits

                            Nothing ->
                                go rest writer
        in
        BW.run (go chars BW.empty)


{-| -}
encode : Bytes -> String
encode bytes =
    let
        w =
            Bytes.width bytes * 8

        go () =
            take w 6
                |> BP.andThen
                    (\bits ->
                        if List.isEmpty bits then
                            BP.succeed []

                        else
                            case Dict.get (BP.bitsToInt bits) intToChar of
                                Nothing ->
                                    BP.fail

                                Just char ->
                                    go () |> BP.map (\chars -> char :: chars)
                    )

        encoded =
            BP.run (go ()) bytes
                |> Maybe.withDefault []
                |> List.Extra.dropWhileRight (\x -> x == '0')
                |> dot
                |> List.reverse
    in
    if List.isEmpty encoded then
        "0w0"

    else
        String.fromList ('0' :: 'w' :: encoded)


dot : List Char -> List Char
dot chars =
    if List.isEmpty chars then
        []

    else if List.length chars <= 5 then
        chars

    else
        List.take 5 chars ++ ('.' :: dot (List.drop 5 chars))


take : Int -> Int -> BP.BitParser (List Int)
take width n =
    if n <= 0 then
        BP.succeed []

    else
        BP.getOffset
            |> BP.andThen
                (\offset ->
                    if offset >= width then
                        BP.succeed []

                    else
                        BP.bit
                            |> BP.andThen
                                (\bit -> take width (n - 1) |> BP.map (\bits -> bit :: bits))
                )


intToChar : Dict.Dict Int Char
intToChar =
    Dict.fromList mapping


charToBits : Dict.Dict Char (List Int)
charToBits =
    let
        intToBits bitsLeft n =
            if bitsLeft > 0 then
                Bitwise.and 1 n :: intToBits (bitsLeft - 1) (Bitwise.shiftRightBy 1 n)

            else
                []
    in
    mapping |> List.map (\( x, y ) -> ( y, intToBits 6 x )) |> Dict.fromList


mapping : List ( number, Char )
mapping =
    [ ( 0, '0' )
    , ( 1, '1' )
    , ( 2, '2' )
    , ( 3, '3' )
    , ( 4, '4' )
    , ( 5, '5' )
    , ( 6, '6' )
    , ( 7, '7' )
    , ( 8, '8' )
    , ( 9, '9' )
    , ( 10, 'a' )
    , ( 11, 'b' )
    , ( 12, 'c' )
    , ( 13, 'd' )
    , ( 14, 'e' )
    , ( 15, 'f' )
    , ( 16, 'g' )
    , ( 17, 'h' )
    , ( 18, 'i' )
    , ( 19, 'j' )
    , ( 20, 'k' )
    , ( 21, 'l' )
    , ( 22, 'm' )
    , ( 23, 'n' )
    , ( 24, 'o' )
    , ( 25, 'p' )
    , ( 26, 'q' )
    , ( 27, 'r' )
    , ( 28, 's' )
    , ( 29, 't' )
    , ( 30, 'u' )
    , ( 31, 'v' )
    , ( 32, 'w' )
    , ( 33, 'x' )
    , ( 34, 'y' )
    , ( 35, 'z' )
    , ( 36, 'A' )
    , ( 37, 'B' )
    , ( 38, 'C' )
    , ( 39, 'D' )
    , ( 40, 'E' )
    , ( 41, 'F' )
    , ( 42, 'G' )
    , ( 43, 'H' )
    , ( 44, 'I' )
    , ( 45, 'J' )
    , ( 46, 'K' )
    , ( 47, 'L' )
    , ( 48, 'M' )
    , ( 49, 'N' )
    , ( 50, 'O' )
    , ( 51, 'P' )
    , ( 52, 'Q' )
    , ( 53, 'R' )
    , ( 54, 'S' )
    , ( 55, 'T' )
    , ( 56, 'U' )
    , ( 57, 'V' )
    , ( 58, 'W' )
    , ( 59, 'X' )
    , ( 60, 'Y' )
    , ( 61, 'Z' )
    , ( 62, '-' )
    , ( 63, '~' )
    ]
