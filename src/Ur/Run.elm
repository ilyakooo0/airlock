module Ur.Run exposing (Model, Msg, application)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Either exposing (Either(..))
import Html
import Json.Decode as JD
import Task
import Time
import Ur exposing (Agent, Path)
import Ur.Cmd
import Ur.Cmd.Internal
import Ur.Constructor as C
import Ur.Deconstructor as D
import Ur.Phonemic exposing (Ship)
import Ur.Requests exposing (..)
import Ur.Sub
import Ur.Sub.Internal
import Ur.Uw
import Url exposing (Url)


type alias SubDict msg =
    Dict
        -- (ship, agent, path)
        ( String, String, List String )
        { deconstructor : D.Deconstructor (msg -> msg) msg
        , number : Int
        }


type alias Flags =
    { uid : String }


type alias Model app msg =
    { subscriptions : SubDict msg
    , subscriptionIntMapping : Dict Int ( String, String, List String )
    , app : app
    , connected : Bool
    , eventId : Int
    , flags : Flags
    , requestsToRetry : List UrbitRequest
    }


type Msg msg
    = AppMsg msg
    | EventSourceMsg JD.Value
    | FailedRequest (List UrbitRequest)
    | Noop
    | OpenConnection
    | NeedsActivation
    | RetryRequests


application :
    { init : Url -> Nav.Key -> ( model, Ur.Cmd.Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Ur.Cmd.Cmd msg )
    , subscriptions : model -> Sub msg
    , urbitSubscriptions : model -> Ur.Sub.Sub msg
    , onUrlRequest : UrlRequest -> msg
    , onUrlChange : Url -> msg
    , createEventSource : String -> Cmd (Msg msg)
    , onEventSourceMsg : (JD.Value -> Msg msg) -> Sub (Msg msg)
    , urbitUrl : model -> String
    }
    -> Program Flags (Model model msg) (Msg msg)
application inp =
    let
        { init, view, onUrlRequest, onUrlChange, onEventSourceMsg } =
            inp
    in
    Browser.application
        { init =
            \flags u key ->
                let
                    ( app, appCmds ) =
                        init u key

                    { subscriptions, eventId, subscriptionRequests, subscriptionIntMapping } =
                        processUrSubs
                            0
                            Dict.empty
                            (inp.urbitSubscriptions app |> (\(Ur.Sub.Internal.Sub x) -> x))

                    ( eventId_, cmds, urReqs ) =
                        processCmd eventId appCmds

                    url =
                        inp.urbitUrl app ++ "/~/channel/" ++ flags.uid
                in
                ( { subscriptions = subscriptions
                  , subscriptionIntMapping = subscriptionIntMapping
                  , app = app
                  , connected = False
                  , eventId = eventId_
                  , flags = flags
                  , requestsToRetry = []
                  }
                , [ cmds
                  , pureCmd NeedsActivation
                  , send
                        { requests = urReqs ++ subscriptionRequests
                        , url = url
                        , error = Noop
                        , success = Noop
                        }
                  ]
                    |> Cmd.batch
                )
        , view =
            \model ->
                view model.app
                    |> (\{ body, title } -> { title = title, body = body |> List.map (Html.map AppMsg) })
        , update = update inp
        , subscriptions =
            \model ->
                Sub.batch
                    [ inp.subscriptions model.app |> Sub.map AppMsg
                    , onEventSourceMsg EventSourceMsg
                    , if List.isEmpty model.requestsToRetry then
                        Sub.none

                      else
                        Time.every 1000 (always RetryRequests)
                    ]
        , onUrlRequest = \req -> onUrlRequest req |> AppMsg
        , onUrlChange = \url -> onUrlChange url |> AppMsg
        }


update :
    { r
        | update : msg -> app -> ( app, Ur.Cmd.Cmd msg )
        , createEventSource : String -> Cmd (Msg msg)
        , urbitUrl : app -> String
        , urbitSubscriptions : app -> Ur.Sub.Sub msg
    }
    -> Msg msg
    -> Model app msg
    -> ( Model app msg, Cmd (Msg msg) )
update inp msg model =
    let
        url =
            inp.urbitUrl model.app ++ "/~/channel/" ++ model.flags.uid
    in
    case msg of
        AppMsg msg_ ->
            let
                ( appModel, appCmds ) =
                    inp.update msg_ model.app

                { subscriptions, eventId, subscriptionRequests, subscriptionIntMapping } =
                    processUrSubs
                        model.eventId
                        model.subscriptions
                        (inp.urbitSubscriptions model.app |> (\(Ur.Sub.Internal.Sub x) -> x))

                ( eventId_, cmds, urReqs ) =
                    processCmd eventId appCmds
            in
            ( { model
                | app = appModel
                , eventId = eventId_
                , subscriptions = subscriptions
                , subscriptionIntMapping = model.subscriptionIntMapping |> Dict.union subscriptionIntMapping
              }
            , Cmd.batch
                [ cmds
                , Ur.Requests.send
                    { url = url
                    , error = Noop
                    , success = Noop
                    , requests = urReqs
                    }
                , send
                    { requests = subscriptionRequests
                    , url = url
                    , error = subscriptionRequests |> List.map (\( _, x ) -> x) |> FailedRequest
                    , success = Noop
                    }
                ]
            )

        EventSourceMsg value ->
            case JD.decodeValue (JD.field "message" JD.string) value of
                Ok string ->
                    case
                        D.runBytes
                            (D.cell D.int (D.cell D.cord D.tar)
                                |> D.map (\a b c -> ( a, b, c ))
                            )
                            (Ur.Uw.decode string)
                    of
                        Just ( messageId, messageType, rest ) ->
                            let
                                ( eventId, ackReqs ) =
                                    tag model.eventId [ Ack messageId ]

                                ackCmd =
                                    send
                                        { requests = ackReqs
                                        , url = url
                                        , success = Noop
                                        , error = ackReqs |> List.map (\( _, x ) -> x) |> FailedRequest
                                        }

                                model_ =
                                    { model | eventId = eventId }
                            in
                            case messageType of
                                "watch-ack" ->
                                    -- Not sure what to do. Assume things are fine.
                                    ( model_, ackCmd )

                                "poke-ack" ->
                                    -- Not sure what to do.
                                    ( model_, ackCmd )

                                "fact" ->
                                    case
                                        Dict.get messageId model.subscriptionIntMapping
                                            |> Maybe.andThen (\key -> Dict.get key model.subscriptions)
                                    of
                                        Just { deconstructor } ->
                                            case D.run (D.cell D.ignore (D.cell D.ignore deconstructor)) rest of
                                                Just subMsg ->
                                                    ( model_, pureCmd (AppMsg subMsg) )

                                                -- Got gargbage
                                                Nothing ->
                                                    ( model_, ackCmd )

                                        -- Got a fact for a subscription we do not hold
                                        Nothing ->
                                            ( model_, ackCmd )

                                _ ->
                                    ( model_, ackCmd )

                        -- got something we don't expect
                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    case JD.decodeValue (JD.field "error" JD.value) value of
                        Ok _ ->
                            ( { model | connected = False }, Cmd.none )

                        Err _ ->
                            -- we got garbage
                            ( model, Cmd.none )

        NeedsActivation ->
            let
                ( eventId, reqs ) =
                    [ Poke { ship = "~zod", agent = "hood", mark = "helm-hi", noun = C.cord "Opening airlock!" } ]
                        |> tag model.eventId
            in
            ( { model | eventId = eventId }
            , send { url = url, requests = reqs, success = OpenConnection, error = NeedsActivation }
            )

        Noop ->
            ( model, Cmd.none )

        FailedRequest reqs ->
            ( { model | requestsToRetry = reqs ++ model.requestsToRetry }, Cmd.none )

        OpenConnection ->
            ( { model | connected = True }, inp.createEventSource url )

        RetryRequests ->
            let
                ( eventId, reqs ) =
                    model.requestsToRetry |> tag model.eventId
            in
            ( { model | eventId = eventId, requestsToRetry = [] }
            , send
                { url = url
                , error = FailedRequest model.requestsToRetry
                , success = Noop
                , requests = reqs
                }
            )


processCmd : EventId -> Ur.Cmd.Cmd msg -> ( EventId, Cmd (Msg msg), List ( EventId, UrbitRequest ) )
processCmd eventId urCmds =
    let
        ( cmds, reqs ) =
            urCmds
                |> List.map
                    (\x ->
                        case x of
                            Ur.Cmd.Internal.Cmd cmd ->
                                cmd |> Cmd.map AppMsg |> Left

                            Ur.Cmd.Internal.Poke p ->
                                Ur.Requests.Poke p |> Right
                    )
                |> Either.partition

        ( newEventId, urReqs ) =
            reqs |> tag eventId
    in
    ( newEventId, Cmd.batch cmds, urReqs )


pureCmd : msg -> Cmd msg
pureCmd msg =
    Task.succeed msg |> Task.perform identity



-- processUrSubs : EventId -> { a | subscriptions : Dict ( Ship, Agent, Path ) b } -> Dict c d -> number


processUrSubs :
    EventId
    -> Dict ( Ship, Agent, Path ) { deconstructor : d, number : EventId }
    -> Dict ( Ship, Agent, Path ) d
    ->
        { subscriptions : Dict ( Ship, Agent, Path ) { deconstructor : d, number : EventId }
        , eventId : EventId
        , subscriptionRequests : List ( EventId, UrbitRequest )
        , subscriptionIntMapping : Dict EventId ( Ship, Agent, Path )
        }
processUrSubs eventId existingSubscriptions urbitSubs_ =
    let
        urbitSubs =
            urbitSubs_
                |> Dict.map (\_ deconstructor -> { deconstructor = deconstructor })

        ( eventId_, newSubscriptionActions ) =
            Dict.diff urbitSubs existingSubscriptions
                |> Dict.toList
                |> List.map (\( address, _ ) -> ( Subscribe address, address ))
                |> tag eventId

        removedSubscriptions =
            Dict.diff existingSubscriptions urbitSubs

        ( eventId__, removedSubscriptionActions ) =
            removedSubscriptions
                |> Dict.toList
                |> List.map (\( _, { number } ) -> Unsubscribe number)
                |> tag eventId_

        keyToNumber =
            newSubscriptionActions |> List.map (\( a, ( _, b ) ) -> ( b, a )) |> Dict.fromList

        newSubscriptions =
            Dict.merge
                (\_ _ x -> x)
                (\key number { deconstructor } ->
                    Dict.insert key
                        { deconstructor = deconstructor
                        , number = number
                        }
                )
                (\_ _ x -> x)
                keyToNumber
                urbitSubs
                Dict.empty
    in
    { subscriptions = Dict.diff existingSubscriptions removedSubscriptions |> Dict.union newSubscriptions
    , subscriptionIntMapping =
        newSubscriptions
            |> Dict.toList
            |> List.map (\( key, { number } ) -> ( number, key ))
            |> Dict.fromList
    , eventId = eventId__
    , subscriptionRequests =
        removedSubscriptionActions ++ (newSubscriptionActions |> List.map (\( id, ( req, _ ) ) -> ( id, req )))
    }
