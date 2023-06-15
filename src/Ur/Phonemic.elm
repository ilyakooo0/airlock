module Ur.Phonemic exposing (Ship, fromString)

import BigInt exposing (toHexString)
import BigInt.Bytes
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
            toBigInt atom |> BigInt.Bytes.encode

        Err _ ->
            Bytes.Extra.empty
