module Ur.Sub exposing (Sub, subscribe, none, batch)

{-| This module is conceptually similar to `Platform.Sub`, but also you to subscribe to Urbit channels.

@docs Sub, subscribe, none, batch

-}

import Dict
import Ur.Deconstructor as D
import Ur.Sub.Internal


{-| -}
type alias Sub msg =
    Ur.Sub.Internal.Sub msg


{-| -}
subscribe : { ship : String, app : String, path : List String, deconstructor : D.Deconstructor (msg -> msg) msg } -> Sub msg
subscribe { ship, app, path, deconstructor } =
    Dict.singleton ( ship, app, path ) deconstructor |> Ur.Sub.Internal.Sub


{-| -}
none : Sub msg
none =
    Ur.Sub.Internal.Sub Dict.empty


{-| -}
batch : List (Sub msg) -> Sub msg
batch subs =
    subs |> List.map (\(Ur.Sub.Internal.Sub dict) -> dict) |> List.foldl Dict.union Dict.empty |> Ur.Sub.Internal.Sub
