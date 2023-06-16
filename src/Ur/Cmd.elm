module Ur.Cmd exposing
    ( Cmd
    , batch
    , cmd
    , none
    , poke
    )

import Ur exposing (Agent, Mark, Noun)
import Ur.Cmd.Internal
import Ur.Phonemic exposing (Ship)


type alias Cmd msg =
    List (Ur.Cmd.Internal.Cmd msg)


none : Cmd msg
none =
    []


poke : { ship : Ship, agent : Agent, mark : Mark, noun : Noun } -> Cmd msg
poke p =
    [ Ur.Cmd.Internal.Poke p ]


cmd : Cmd.Cmd msg -> Cmd msg
cmd c =
    [ Ur.Cmd.Internal.Cmd c ]


batch : List (Cmd msg) -> Cmd msg
batch =
    List.concat
