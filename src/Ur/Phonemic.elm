module Ur.Phonemic exposing (Ship, fromString)

import BigInt exposing (toHexString)
import Bytes.Extra
import Hex.Convert as Hex
import Ur exposing (Atom, Noun(..))
import Urbit.Encoding.Atom exposing (toBigInt)
import Urbit.Encoding.Phonemic exposing (..)


{-| A ship name like `~zod` or `~racfer-hattes`.

Also know as `@p`.

-}
type alias Ship =
    String


{-| Converts a string like '~zod' into an Atom.
-}
fromString : Ship -> Atom
fromString s =
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
            Hex.toBytes paddedHexString |> Maybe.withDefault Bytes.Extra.empty

        Err _ ->
            Bytes.Extra.empty
