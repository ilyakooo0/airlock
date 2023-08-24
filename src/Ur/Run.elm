module Ur.Run exposing (element, document, application, Program, Model, Msg)

{-| This module contains functions to run your application with Urbit integration.

@docs element, document, application, Program, Model, Msg

-}

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Either exposing (Either(..))
import Html exposing (Html)
import Json.Decode as JD
import Task
import Time
import Ur.Cmd
import Ur.Cmd.Internal
import Ur.Constructor as C
import Ur.Deconstructor as D
import Ur.NounDiff exposing (Patch, deconstructPatch)
import Ur.Requests exposing (..)
import Ur.Sub
import Ur.Sub.Internal
import Ur.Types exposing (..)
import Ur.Uw
import Url exposing (Url)


type alias SubDict msg =
    Dict
        -- (ship, agent, path)
        ( String, String, List String )
        { deconstructor : D.Deconstructor msg
        , number : Int
        , sink : Bool
        }


type alias Flags =
    { uid : String }


{-| An Urbit wrapper around your application model.
-}
type alias Model app msg =
    { subscriptions : SubDict msg
    , subscriptionIntMapping : Dict Int ( String, String, List String )
    , app : app
    , connected : Bool
    , eventId : Int
    , flags : Flags
    , requestsToRetry : List UrbitRequest
    , sinks : Dict Int Noun
    }


{-| An Urbit wrapper around your application messages.
-}
type Msg msg
    = AppMsg msg
    | EventSourceMsg JD.Value
    | FailedRequest (List UrbitRequest)
    | Noop
    | OpenConnection
    | NeedsActivation
    | RetryRequests


{-| A wrapper around your application `Program`.
-}
type alias Program model msg =
    Platform.Program Flags (Model model msg) (Msg msg)


{-| The same as `Browser.element` but with urbit stuff added around it.
-}
element :
    { init : ( model, Ur.Cmd.Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Ur.Cmd.Cmd msg )
    , subscriptions : model -> Sub msg
    , urbitSubscriptions : model -> Ur.Sub.Sub msg
    , createEventSource : String -> Cmd (Msg msg)
    , onEventSourceMsg : (JD.Value -> Msg msg) -> Sub (Msg msg)
    , urbitUrl : model -> String
    }
    -> Program model msg
element inp =
    Browser.element
        { init = init inp inp.init
        , view = \model -> inp.view model.app |> Html.map AppMsg
        , update = update inp
        , subscriptions = subscriptions inp
        }


{-| The same as `Browser.element` but with urbit stuff added around it.
-}
document :
    { init : ( model, Ur.Cmd.Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Ur.Cmd.Cmd msg )
    , subscriptions : model -> Sub msg
    , urbitSubscriptions : model -> Ur.Sub.Sub msg
    , createEventSource : String -> Cmd (Msg msg)
    , onEventSourceMsg : (JD.Value -> Msg msg) -> Sub (Msg msg)
    , urbitUrl : model -> String
    }
    -> Program model msg
document inp =
    Browser.document
        { init = init inp inp.init
        , view =
            \model ->
                inp.view model.app
                    |> (\{ body, title } -> { title = title, body = body |> List.map (Html.map AppMsg) })
        , update = update inp
        , subscriptions = subscriptions inp
        }


{-| The same as `Browser.element` but with urbit stuff added around it.
-}
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
    -> Program model msg
application inp =
    let
        { view, onUrlRequest, onUrlChange } =
            inp
    in
    Browser.application
        { init = \flags url key -> init inp (inp.init url key) flags
        , view =
            \model ->
                view model.app
                    |> (\{ body, title } -> { title = title, body = body |> List.map (Html.map AppMsg) })
        , update = update inp
        , subscriptions = subscriptions inp
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

                subsResult =
                    processUrSubs
                        model.eventId
                        model.subscriptions
                        (inp.urbitSubscriptions appModel |> (\(Ur.Sub.Internal.Sub x) -> x))

                ( eventId_, cmds, urReqs ) =
                    processCmd subsResult.eventId appCmds
            in
            ( { model
                | app = appModel
                , eventId = eventId_
                , subscriptions = subsResult.subscriptions
                , subscriptionIntMapping = model.subscriptionIntMapping |> Dict.union subsResult.subscriptionIntMapping
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
                    { requests = subsResult.subscriptionRequests
                    , url = url
                    , error = subsResult.subscriptionRequests |> List.map (\( _, x ) -> x) |> FailedRequest
                    , success = Noop
                    }
                ]
            )

        EventSourceMsg value ->
            case JD.decodeValue (JD.field "message" JD.string) value of
                Ok string ->
                    case
                        D.runBytes
                            (D.cell D.int (D.cell D.cord D.tar))
                            (Ur.Uw.decode string)
                    of
                        Just ( messageId, ( messageType, rest ) ) ->
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
                                        Just { deconstructor, sink } ->
                                            if sink then
                                                case D.run (D.cell D.ignore (D.cell D.ignore deconstructSink) |> D.map (\( (), ( (), s ) ) -> s)) rest of
                                                    Just (Flush noun) ->
                                                        ( { model | sinks = Dict.insert messageId noun model.sinks }
                                                        , case D.run deconstructor noun of
                                                            Just subMsg ->
                                                                pureCmd (AppMsg subMsg)

                                                            -- Got garbage
                                                            Nothing ->
                                                                Cmd.none
                                                        )

                                                    Just (Drain patch) ->
                                                        case Dict.get messageId model.sinks of
                                                            Just oldNoun ->
                                                                let
                                                                    newNoun =
                                                                        Ur.NounDiff.apply patch oldNoun
                                                                in
                                                                ( { model | sinks = Dict.insert messageId newNoun model.sinks }
                                                                , case D.run deconstructor newNoun of
                                                                    Just subMsg ->
                                                                        pureCmd (AppMsg subMsg)

                                                                    Nothing ->
                                                                        Cmd.none
                                                                )

                                                            Nothing ->
                                                                ( model, Cmd.none )

                                                    -- Got garbage
                                                    Nothing ->
                                                        ( model, Cmd.none )

                                            else
                                                case D.run (D.cell D.ignore (D.cell D.ignore deconstructor) |> D.map (\( (), ( (), m ) ) -> m)) rest of
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
                    [ Poke { ship = "", agent = "hood", mark = "helm-hi", noun = C.cord "Opening airlock!" } ]
                        |> tag model.eventId
            in
            ( { model | eventId = eventId }
            , send { url = url, requests = reqs, success = OpenConnection, error = Noop }
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


type Sink
    = Flush Noun
    | Drain Patch


deconstructSink : D.Deconstructor Sink
deconstructSink =
    D.oneOf
        [ D.cell (D.const D.cord "flush") D.tar |> D.map (\( (), n ) -> Flush n)
        , D.cell (D.const D.cord "drain") (D.lazy deconstructPatch) |> D.map (\( (), patch ) -> Drain patch)
        ]


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


processUrSubs :
    EventId
    -> Dict ( Ship, Agent, Path ) { deconstructor : d, number : EventId, sink : Bool }
    -> Dict ( Ship, Agent, Path ) { deconstructor : d, sink : Bool }
    ->
        { subscriptions : Dict ( Ship, Agent, Path ) { deconstructor : d, number : EventId, sink : Bool }
        , eventId : EventId
        , subscriptionRequests : List ( EventId, UrbitRequest )
        , subscriptionIntMapping : Dict EventId ( Ship, Agent, Path )
        }
processUrSubs eventId existingSubscriptions urbitSubs_ =
    let
        urbitSubs =
            urbitSubs_
                |> Dict.map (\_ { deconstructor, sink } -> { deconstructor = deconstructor, sink = sink })

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
                (\key number { deconstructor, sink } ->
                    Dict.insert key
                        { deconstructor = deconstructor
                        , number = number
                        , sink = sink
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


init :
    { a | urbitSubscriptions : model -> Ur.Sub.Sub msg, urbitUrl : model -> String }
    -> ( model, Ur.Cmd.Cmd msg )
    -> Flags
    ->
        ( Model model msg
        , Cmd (Msg msg)
        )
init inp ( app, appCmds ) flags =
    let
        subsResult =
            processUrSubs
                0
                Dict.empty
                (inp.urbitSubscriptions app |> (\(Ur.Sub.Internal.Sub x) -> x))

        ( eventId_, cmds, urReqs ) =
            processCmd subsResult.eventId appCmds

        url =
            inp.urbitUrl app ++ "/~/channel/" ++ flags.uid
    in
    ( { subscriptions = subsResult.subscriptions
      , sinks = Dict.empty
      , subscriptionIntMapping = subsResult.subscriptionIntMapping
      , app = app
      , connected = False
      , eventId = eventId_
      , flags = flags
      , requestsToRetry = []
      }
    , [ cmds
      , pureCmd NeedsActivation
      , send
            { requests = urReqs ++ subsResult.subscriptionRequests
            , url = url
            , error = Noop
            , success = Noop
            }
      ]
        |> Cmd.batch
    )


subscriptions :
    { a | subscriptions : b -> Sub msg, onEventSourceMsg : (JD.Value -> Msg c) -> Sub (Msg msg) }
    -> { d | app : b, requestsToRetry : List e, connected : Bool }
    -> Sub (Msg msg)
subscriptions inp model =
    Sub.batch
        [ inp.subscriptions model.app |> Sub.map AppMsg
        , inp.onEventSourceMsg EventSourceMsg
        , if List.isEmpty model.requestsToRetry then
            Sub.none

          else
            Time.every 1000 (always RetryRequests)
        , if not model.connected then
            Time.every 10000 (always NeedsActivation)

          else
            Sub.none
        ]
