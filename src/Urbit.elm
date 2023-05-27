module Urbit exposing (Noun(..), mat, rub)

import BitParser as BP exposing (BitParser)
import BitWriter as BW exposing (BitWriter)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Extra as Bytes
import List.Extra as List


type Noun
    = Cell ( Noun, Noun )
    | Atom Bytes


isSig : Bytes -> Bool
isSig bytes =
    Bytes.toByteValues bytes |> List.all (\x -> x == 0)


mat : Bytes -> BitWriter -> BitWriter
mat bytes writer =
    if isSig bytes then
        writer |> BW.bit 1

    else
        let
            bits =
                bytesToBits bytes |> List.dropWhileRight (\x -> x == 0)

            lengthBits =
                bits |> List.length |> intToBits |> List.reverse |> List.drop 1 |> List.reverse
        in
        writer
            |> BW.bit 0
            |> BW.bits (List.repeat (List.length lengthBits) 0)
            |> BW.bit 1
            |> BW.bits lengthBits
            |> BW.bits bits


rub : BitParser Bytes
rub =
    BP.bit
        |> BP.andThen
            (\zeroBit ->
                if zeroBit == 1 then
                    BP.succeed Bytes.empty

                else
                    let
                        countZeros n =
                            BP.bit
                                |> BP.andThen
                                    (\b ->
                                        if b == 0 then
                                            countZeros (n + 1)

                                        else
                                            BP.succeed n
                                    )
                    in
                    countZeros 0
                        |> BP.andThen
                            (\lengthOfLength ->
                                BP.rawBits lengthOfLength
                                    |> BP.andThen
                                        (\preLengthRawBits ->
                                            let
                                                length =
                                                    BP.bitsToInt (preLengthRawBits ++ [ 1 ])
                                            in
                                            BP.bits length
                                        )
                            )
            )


bytesToBits : Bytes -> List Int
bytesToBits bytes =
    case
        BP.run (BP.rawBits (Bytes.width bytes * 8)) bytes
    of
        Nothing ->
            []

        Just bits ->
            bits


intToBits : Int -> List Int
intToBits n =
    if n <= 0 then
        []

    else
        Bitwise.and 1 n :: intToBits (Bitwise.shiftRightBy 1 n)
