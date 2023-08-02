module Ur.Requests exposing
    ( EventId
    , UrbitRequest(..)
    , send
    , sendTask
    , tag
    , toNoun
    )

import Http
import Task exposing (Task)
import Ur exposing (..)
import Ur.Constructor as C
import Ur.Jam exposing (jam)
import Ur.Phonemic
import Ur.Types exposing (..)
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


send :
    { url : String
    , error : msg
    , success : msg
    , requests : List ( EventId, UrbitRequest )
    }
    -> Cmd msg
send inp =
    sendTask inp |> Task.perform identity


{-| `requests` should the result of calling `tag`
-}
sendTask :
    { url : String
    , error : msg
    , success : msg
    , requests : List ( EventId, UrbitRequest )
    }
    -> Task a msg
sendTask { url, error, success, requests } =
    if List.isEmpty requests then
        Task.succeed success

    else
        Http.riskyTask
            { method = "PUT"
            , headers = []
            , url = url
            , body =
                requests
                    |> List.map (uncurry toNoun)
                    |> C.listOf identity
                    |> jam
                    |> Ur.Uw.encode
                    |> Http.stringBody "application/x-urb-jam"
            , resolver =
                Http.bytesResolver
                    (\resp ->
                        case resp of
                            Http.GoodStatus_ _ _ ->
                                Ok success

                            _ ->
                                Ok error
                    )
            , timeout = Nothing
            }


type alias EventId =
    Int


toNoun : EventId -> UrbitRequest -> Noun
toNoun eventId req =
    case req of
        Subscribe ( ship, app, path ) ->
            C.cell (C.cord "subscribe") <|
                C.cell (C.int eventId) <|
                    C.cell (Atom (Ur.Phonemic.fromString ship)) <|
                        C.cell (C.cord app) (C.listOf C.cord path)

        Unsubscribe subId ->
            C.cell (C.cord "unsubscribe") <|
                C.cell (C.int eventId) (C.int subId)

        Poke { ship, agent, mark, noun } ->
            C.cell (C.cord "poke") <|
                C.cell (C.int eventId) <|
                    C.cell (Atom (Ur.Phonemic.fromString ship)) <|
                        C.cell (C.cord agent) <|
                            C.cell (C.cord mark) <|
                                noun

        Ack number ->
            C.cell (C.cord "ack") (C.int number)


uncurry : (a -> b -> c) -> (( a, b ) -> c)
uncurry f ( a, b ) =
    f a b
