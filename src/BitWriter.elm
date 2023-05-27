module BitWriter exposing
    ( BitWriter
    , bit
    , bits
    , empty
    , run
    )

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Encode as BE


empty : BitWriter
empty =
    BitWriter { running = Nothing, collected = BE.sequence [] }


run : BitWriter -> Bytes
run (BitWriter { running, collected }) =
    case running of
        Nothing ->
            BE.encode collected

        Just { value } ->
            BE.sequence [ collected, BE.unsignedInt8 value ] |> BE.encode


type BitWriter
    = BitWriter
        { running :
            Maybe
                { value : Int
                , length : Int
                }
        , collected : BE.Encoder
        }


{-| If the given int is longer than 1 bit the behaviour is undefined
-}
bit : Int -> BitWriter -> BitWriter
bit b (BitWriter { running, collected }) =
    case running of
        Nothing ->
            BitWriter { running = Just { value = b, length = 1 }, collected = collected }

        Just { value, length } ->
            let
                newValue =
                    Bitwise.or (Bitwise.shiftLeftBy length b) value
            in
            if length == 7 then
                BitWriter { running = Nothing, collected = BE.sequence [ collected, BE.unsignedInt8 newValue ] }

            else
                BitWriter { running = Just { value = newValue, length = length + 1 }, collected = collected }



-- PERF: Collect 8-bit chunks at boundaries and add them without going through bit


bits : List Int -> BitWriter -> BitWriter
bits bs writer =
    case bs of
        [] ->
            writer

        b :: bb ->
            bits bb (bit b writer)



-- -- PERF: don't go through BitParser to not hold all bits in memory
-- bytes : Bytes -> BitWriter -> BitWriter
-- bytes bite writer =
--     case BP.run (BP.rawBits (Bytes.width bite * 8)) bite of
--         -- this is bad, but shouldn't happen
--         Nothing ->
--             writer
--         Just bs ->
--             bits bs writer
