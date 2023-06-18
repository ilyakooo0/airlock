module Ur.Cmd exposing (Cmd, poke, cmd, none, batch)

{-| This module is conceptually similar to `Platform.Cmd`, but also allows you to send Urbit requests.

@docs Cmd, poke, cmd, none, batch

-}

import Ur.Cmd.Internal
import Ur.Types exposing (Agent, Mark, Noun, Ship)


{-| -}
type alias Cmd msg =
    List (Ur.Cmd.Internal.Cmd msg)


{-| -}
none : Cmd msg
none =
    []


{-| -}
poke : { ship : Ship, agent : Agent, mark : Mark, noun : Noun } -> Cmd msg
poke p =
    [ Ur.Cmd.Internal.Poke p ]


{-| -}
cmd : Cmd.Cmd msg -> Cmd msg
cmd c =
    [ Ur.Cmd.Internal.Cmd c ]


{-| -}
batch : List (Cmd msg) -> Cmd msg
batch =
    List.concat
