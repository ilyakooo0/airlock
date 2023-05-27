module Urbit exposing (..)

import BitParser as BP exposing (BitParser)
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE


type Noun
    = Cell ( Noun, Noun )
    | Atom Bytes


rub : BitParser Bytes
rub =
    BP.bit
        |> BP.andThen
            (\zeroBit ->
                if zeroBit == 1 then
                    BP.succeed (BE.unsignedInt8 0 |> BE.encode)

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
                                                    BP.bitsToInt (1 :: preLengthRawBits)
                                            in
                                            BP.bits length
                                        )
                            )
            )



-- met : BitParser Bytes
-- met =
--     1
