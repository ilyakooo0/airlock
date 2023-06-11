module Ur.Sub exposing (Sub(..), batch, none, subscribe)

import Dict exposing (Dict)
import Ur.Deconstructor as D


type Sub msg
    = Sub
        (Dict
            -- key is (ship, app, path)
            ( String, String, List String )
            (D.Deconstructor (msg -> msg) msg)
        )


subscribe : { ship : String, app : String, path : List String, deconstructor : D.Deconstructor (msg -> msg) msg } -> Sub msg
subscribe { ship, app, path, deconstructor } =
    Dict.singleton ( ship, app, path ) deconstructor |> Sub


none : Sub msg
none =
    Sub Dict.empty


batch : List (Sub msg) -> Sub msg
batch subs =
    subs |> List.map (\(Sub dict) -> dict) |> List.foldl Dict.union Dict.empty |> Sub
