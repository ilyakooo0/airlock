module Ur.Sub.Internal exposing (Sub(..))

import Dict exposing (Dict)
import Ur.Deconstructor as D


type Sub msg
    = Sub
        (Dict
            -- key is (ship, app, path)
            ( String, String, List String )
            (D.Deconstructor (msg -> msg) msg)
        )
