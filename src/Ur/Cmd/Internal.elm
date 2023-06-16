module Ur.Cmd.Internal exposing (Cmd(..))

import Ur exposing (Agent, Mark, Noun)
import Ur.Phonemic exposing (Ship)


type Cmd msg
    = Poke { ship : Ship, agent : Agent, mark : Mark, noun : Noun }
    | Cmd (Cmd.Cmd msg)
