module Ur.Cmd.Internal exposing (Cmd(..))

import Ur.Types exposing (Agent, Mark, Noun, Ship)


type Cmd msg
    = Poke { ship : Ship, agent : Agent, mark : Mark, noun : Noun }
    | Cmd (Cmd.Cmd msg)
