module Ur.Sub.Internal exposing (Sub(..))

import Dict exposing (Dict)
import Ur exposing (Agent, Path)
import Ur.Deconstructor as D
import Ur.Phonemic exposing (Ship)


type Sub msg
    = Sub
        (Dict
            -- key is (ship, app, path)
            ( Ship, Agent, Path )
            (D.Deconstructor (msg -> msg) msg)
        )
