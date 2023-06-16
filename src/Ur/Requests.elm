module Ur.Requests exposing
    ( EventId
    , UrbitRequest(..)
    , scry
    , scryTask
    , send
    , sendTask
    , tag
    , toNoun
    )

import Http
import Task exposing (Task)
import Ur exposing (..)
import Ur.Constructor as C
import Ur.Deconstructor as D
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
                    |> Ur.jam
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


scry :
    { url : String
    , agent : Agent
    , path : Path
    , error : msg
    , success : D.Deconstructor (msg -> msg) msg
    }
    -> Cmd msg
scry args =
    scryTask args |> Task.perform identity


scryTask :
    { url : String
    , agent : Agent
    , path : Path
    , error : msg
    , success : D.Deconstructor (msg -> msg) msg
    }
    -> Task a msg
scryTask { url, agent, path, error, success } =
    Http.riskyTask
        { method = "GET"
        , headers = []
        , url = url ++ "/~/scry/" ++ agent ++ "/" ++ String.join "/" path ++ ".jam"
        , body = Http.emptyBody
        , resolver =
            Http.bytesResolver
                (\resp ->
                    case resp of
                        Http.GoodStatus_ _ bytes ->
                            case D.runBytes success bytes of
                                Just msg ->
                                    Ok msg

                                Nothing ->
                                    Ok error

                        _ ->
                            Ok error
                )
        , timeout = Nothing
        }


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
