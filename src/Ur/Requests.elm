module Ur.Requests exposing
    ( EventId
    , UrbitRequest(..)
    , send
    , tag
    , toNoun
    )

import Http
import Ur exposing (..)
import Ur.Constructor as C
import Ur.Phonemic exposing (Ship)
import Ur.Uw


type UrbitRequest
    = Subscribe ( Ship, Agent, Path )
    | Unsubscribe EventId
    | Poke { ship : Ship, agent : Agent, mark : Mark, noun : Noun }
    | Ack Int


tag : EventId -> List x -> ( EventId, List ( EventId, x ) )
tag eventId reqs =
    case reqs of
        [] ->
            ( eventId, [] )

        req :: rest ->
            tag (eventId + 1) rest |> Tuple.mapSecond (\xs -> ( eventId, req ) :: xs)


{-| `requests` should the result of calling `tag`
-}
send :
    { url : String
    , error : msg
    , success : msg
    , requests : List ( EventId, UrbitRequest )
    }
    -> Cmd msg
send { url, error, success, requests } =
    if List.isEmpty requests then
        Cmd.none

    else
        Http.riskyRequest
            { method = "PUT"
            , headers = []
            , url = url
            , body =
                requests
                    |> List.map (uncurry toNoun)
                    |> C.listOf identity
                    |> Ur.jam
                    |> Ur.Uw.encode
                    |> Http.stringBody "application/x-urb-jam"
            , expect = Http.expectWhatever (result (\_ -> error) (always success))
            , timeout = Nothing
            , tracker = Nothing
            }


type alias EventId =
    Int


toNoun : EventId -> UrbitRequest -> Noun
toNoun eventId req =
    case req of
        Subscribe ( ship, app, path ) ->
            C.cell (C.cord "subscribe") <|
                C.cell (C.int eventId) <|
                    C.cell (Ur.Atom (Ur.Phonemic.fromString ship)) <|
                        C.cell (C.cord app) (C.listOf C.cord path)

        Unsubscribe subId ->
            C.cell (C.cord "usubscribe") <|
                C.cell (C.int eventId) (C.int subId)

        Poke { ship, agent, mark, noun } ->
            C.cell (C.cord "poke") <|
                C.cell (C.int eventId) <|
                    C.cell (Ur.Atom (Ur.Phonemic.fromString ship)) <|
                        C.cell (C.cord agent) <|
                            C.cell (C.cord mark) <|
                                noun

        Ack number ->
            C.cell (C.cord "ack") (C.int number)


uncurry : (a -> b -> c) -> (( a, b ) -> c)
uncurry f ( a, b ) =
    f a b


result : (a -> c) -> (b -> c) -> Result a b -> c
result f g res =
    case res of
        Ok b ->
            g b

        Err a ->
            f a
