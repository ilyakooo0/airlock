module Ur.Phonemic exposing (..)

import BigInt exposing (toHexString)
import Bytes exposing (Bytes)
import Hex.Convert as Hex
import Urbit.Encoding.Atom exposing (toBigInt)
import Urbit.Encoding.Phonemic exposing (..)


p : String -> Maybe Bytes
p s =
    case fromPatp s of
        Ok atom ->
            let
                hexString =
                    toBigInt atom |> toHexString

                paddedHexString =
                    if modBy 2 (String.length hexString) == 0 then
                        hexString

                    else
                        "0" ++ hexString
            in
            Hex.toBytes paddedHexString

        Err _ ->
            Nothing
