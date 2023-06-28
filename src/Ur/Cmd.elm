module Ur.Cmd exposing (Cmd, poke, cmd, none, batch)

{-| This module is conceptually similar to `Platform.Cmd`, but also allows you to send Urbit requests.

@docs Cmd, poke, cmd, none, batch

-}

import Ur.Cmd.Internal
import Ur.Types exposing (Agent, Mark, Noun, Ship)


{-| Like `Cmd` from `Platform.Cmd`, but for Urbit commands.
-}
type alias Cmd msg =
    List (Ur.Cmd.Internal.Cmd msg)


{-| A command that does exactly nothing.
-}
none : Cmd msg
none =
    []


{-| Sends a %poke to a Gall Agent.

    poke
        { ship = "~zod"
        , agent = "journal"
        , mark = "journal-action"
        , noun = C.cell (C.cord "del") (C.bigint id)
        }

-}
poke : { ship : Ship, agent : Agent, mark : Mark, noun : Noun } -> Cmd msg
poke p =
    [ Ur.Cmd.Internal.Poke p ]


{-| Turns a `Platform.Cmd` command into a `Ur.Cmd` command.
-}
cmd : Cmd.Cmd msg -> Cmd msg
cmd c =
    [ Ur.Cmd.Internal.Cmd c ]


{-| Batches multiple commands into one. Similar to `Platform.Cmd.batch`.
-}
batch : List (Cmd msg) -> Cmd msg
batch =
    List.concat
