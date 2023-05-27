module BitParser exposing
    ( BitParser
    , andThen
    , bit
    , bits
    , bitsToInt
    , fail
    , intBits
    , map
    , rawBits
    , run
    , succeed
    )

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE


run : BitParser a -> Bytes -> Maybe a
run (BitParser f) bytes =
    BD.decode (f { running = Nothing }) bytes |> Maybe.map Tuple.second


type alias BitParserState =
    { running : Maybe { byte : Int, length : Int }
    }


type BitParser a
    = BitParser (BitParserState -> BD.Decoder ( BitParserState, a ))


bit : BitParser Int
bit =
    BitParser
        (\{ running } ->
            case running of
                Nothing ->
                    BD.unsignedInt8
                        |> BD.map
                            (\i ->
                                ( { running = Just { byte = Bitwise.shiftRightBy 1 i, length = 7 } }
                                , Bitwise.and 1 i
                                )
                            )

                Just { byte, length } ->
                    BD.succeed
                        (if length == 1 then
                            ( { running = Nothing }, byte )

                         else
                            ( { running = Just { byte = Bitwise.shiftRightBy 1 byte, length = length - 1 } }
                            , Bitwise.and 1 byte
                            )
                        )
        )


rawBits : Int -> BitParser (List Int)
rawBits n =
    let
        go i =
            if i <= 0 then
                succeed []

            else
                bit |> andThen (\b -> map (\x -> b :: x) (go (i - 1)))
    in
    go n



-- PERF: use `bytes` on byte boundaries


bits : Int -> BitParser Bytes
bits n =
    rawBits n |> map bitsToBytes


intBits : Int -> BitParser Int
intBits n =
    rawBits n |> map bitsToInt


bitsToInt : List Int -> Int
bitsToInt =
    List.foldr (\b acc -> Bitwise.shiftLeftBy 1 acc |> Bitwise.or b) 0


bitsToBytes : List Int -> Bytes
bitsToBytes bs =
    if bs == [] then
        emptyBytes

    else
        let
            go bb =
                if List.length bb > 8 then
                    BE.unsignedInt8 (List.take 8 bb |> bitsToInt) :: go (List.drop 8 bb)

                else
                    [ BE.unsignedInt8 (bb |> bitsToInt) ]
        in
        go bs |> BE.sequence |> BE.encode


emptyBytes : Bytes
emptyBytes =
    BE.encode (BE.sequence [])


andThen : (a -> BitParser b) -> BitParser a -> BitParser b
andThen f (BitParser g) =
    BitParser (\s -> g s |> BD.andThen (\( s1, a ) -> f a |> (\(BitParser h) -> h s1)))


map : (a -> b) -> BitParser a -> BitParser b
map f (BitParser g) =
    BitParser (\s -> g s |> BD.map (Tuple.mapSecond f))


succeed : a -> BitParser a
succeed a =
    BitParser (\state -> BD.succeed ( state, a ))


fail : BitParser a
fail =
    BitParser (always BD.fail)
