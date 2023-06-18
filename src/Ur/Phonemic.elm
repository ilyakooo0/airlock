module Ur.Phonemic exposing (fromString)

{-|

@docs fromString

-}

import BigInt.Bytes
import Bytes.Extra
import Ur.Types exposing (Atom, Noun(..), Ship)
import Urbit.Encoding.Atom exposing (toBigInt)
import Urbit.Encoding.Phonemic exposing (..)


{-| Converts a ship name like `~zod` into an Atom.
-}
fromString : Ship -> Atom
fromString s =
    case fromPatp s of
        Ok atom ->
            toBigInt atom |> BigInt.Bytes.encode

        Err _ ->
            Bytes.Extra.empty
