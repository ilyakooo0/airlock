module Ur.Sub.Internal exposing (Sub(..))

import Dict exposing (Dict)
import Ur.Deconstructor as D
import Ur.Types exposing (Agent, Path, Ship)


type Sub msg
    = Sub
        (Dict
            -- key is (ship, app, path)
            ( Ship, Agent, Path )
            { deconstructor : D.Deconstructor (msg -> msg) msg
            , sink : Bool
            }
        )
