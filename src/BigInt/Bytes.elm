module BigInt.Bytes exposing (decode, encode)

import BigInt exposing (BigInt, toHexString)
import Bytes exposing (Bytes)
import Bytes.Extra
import Hex.Convert as Hex


encode : BigInt -> Bytes
encode x =
    let
        hexString =
            toHexString x

        paddedHexString =
            if modBy 2 (String.length hexString) == 0 then
                hexString

            else
                "0" ++ hexString
    in
    Hex.toBytes paddedHexString
        |> Maybe.map (Bytes.Extra.toByteValues >> List.reverse >> Bytes.Extra.fromByteValues)
        |> Maybe.withDefault Bytes.Extra.empty


decode : Bytes -> BigInt
decode bs =
    bs
        |> Bytes.Extra.toByteValues
        |> List.reverse
        |> Bytes.Extra.fromByteValues
        |> Hex.toString
        |> BigInt.fromHexString
        |> Maybe.withDefault (BigInt.fromInt 0)
