module Ur exposing (logIn, getShipName, scry, scryTask)

{-|


# Simple requests

@docs logIn, getShipName, scry, scryTask

-}

import Http
import Platform exposing (Task)
import Task
import Ur.Deconstructor as D
import Ur.Types exposing (..)


{-| Logs into your Urbit at the given root path and password.

    logIn "http://localhost:8080" "lidlut-tabwed-pillex-ridrup"

-}
logIn : String -> String -> Cmd (Result Http.Error ())
logIn root password =
    Http.riskyRequest
        { url = root ++ "/~/login"
        , method = "POST"
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , body =
            Http.stringBody
                "application/x-www-form-urlencoded; charset=utf-8"
                ("password=" ++ password)
        , expect = Http.expectWhatever identity
        }


{-| Queries the name of the current ship.
Return strings like `~zod` or `~racfer-hattes`.

    getShipName "http://localhost:8080"

-}
getShipName : String -> Cmd (Result Http.Error Ship)
getShipName root =
    Http.riskyRequest
        { url = root ++ "/~/name"
        , method = "GET"
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , expect = Http.expectString identity
        }


{-| Scry an agent at some path

    scry
        { url = "http://localhost:8080"
        , agent = "journal"
        , path = [ "entries", "all" ]
        , error = Noop
        , success =
            D.cell D.ignore
                (D.cell (D.const D.cord "jrnl")
                    (D.list (D.cell D.bigint D.cord |> D.map (\a b -> ( a, b ))))
                    |> D.map GotListings
                )
        }

-}
scry :
    { url : String
    , agent : Agent
    , path : Path
    , error : msg
    , success : D.Deconstructor msg
    }
    -> Cmd msg
scry args =
    scryTask args |> Task.perform identity


{-| Same as `scry` but returns a `Task` instead of a `Cmd`.
-}
scryTask :
    { url : String
    , agent : Agent
    , path : Path
    , error : msg
    , success : D.Deconstructor msg
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
