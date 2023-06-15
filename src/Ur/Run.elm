module Ur.Run exposing (Model, application)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html
import Json.Decode as JD
import Task
import Ur.Constructor as C
import Ur.Deconstructor as D
import Ur.Requests exposing (..)
import Ur.Sub
import Ur.Uw
import Url exposing (Url)


type alias SubDict msg =
    Dict
        -- (ship, app, path)
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


application :
    { init : Url -> Nav.Key -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
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
        { init, view, subscriptions, onUrlRequest, onUrlChange, urbitSubscriptions, createEventSource, onEventSourceMsg, urbitUrl } =
            inp
    in
    Browser.application
        { init =
            \flags u key ->
                let
                    ( app, appCmds ) =
                        init u key
                in
                ( { subscriptions = Dict.empty
                  , subscriptionIntMapping = Dict.empty
                  , app = app
                  , connected = False
                  , eventId = 0
                  , flags = flags
                  , requestsToRetry = []
                  }
                , [ Cmd.map AppMsg appCmds, pureCmd NeedsActivation ] |> Cmd.batch
                )
        , view =
            \model ->
                view model.app
                    |> (\{ body, title } -> { title = title, body = body |> List.map (Html.map AppMsg) })
        , update = update inp
        , subscriptions =
            \model ->
                Sub.batch
                    [ subscriptions model.app |> Sub.map AppMsg
                    , onEventSourceMsg EventSourceMsg
                    ]
        , onUrlRequest = \req -> onUrlRequest req |> AppMsg
        , onUrlChange = \url -> onUrlChange url |> AppMsg
        }


update :
    { r
        | update : msg -> app -> ( app, Cmd msg )
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

                urbitSubs_ =
                    inp.urbitSubscriptions model.app |> (\(Ur.Sub.Sub x) -> x)

                urbitSubs =
                    urbitSubs_
                        |> Dict.map (\_ deconstructor -> { deconstructor = deconstructor })

                ( eventId, newSubscriptionActions ) =
                    Dict.diff urbitSubs model.subscriptions
                        |> Dict.toList
                        |> List.map (\( address, _ ) -> ( Subscribe address, address ))
                        |> tag model.eventId

                removedSubscriptions =
                    Dict.diff model.subscriptions urbitSubs

                ( eventId_, removedSubscriptionActions ) =
                    removedSubscriptions
                        |> Dict.toList
                        |> List.map (\( _, { number } ) -> Unsubscribe number)
                        |> tag eventId

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
            ( { model
                | app = appModel
                , eventId = eventId_
                , subscriptions =
                    Dict.diff model.subscriptions removedSubscriptions
                        |> Dict.union newSubscriptions
                , subscriptionIntMapping =
                    model.subscriptionIntMapping
                        |> Dict.union
                            (newSubscriptions
                                |> Dict.toList
                                |> List.map (\( key, { number } ) -> ( number, key ))
                                |> Dict.fromList
                            )
              }
            , Cmd.batch
                [ appCmds |> Cmd.map AppMsg
                , let
                    requests =
                        removedSubscriptionActions
                            ++ (newSubscriptionActions |> List.map (\( id, ( req, _ ) ) -> ( id, req )))
                  in
                  send
                    { requests = requests
                    , url = url
                    , error = requests |> List.map (\( _, x ) -> x) |> FailedRequest
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
                                        Dict.get messageId model.subscriptionIntMapping |> Maybe.andThen (\key -> Dict.get key model.subscriptions)
                                    of
                                        Just { deconstructor } ->
                                            case D.run (D.cell D.tar deconstructor |> D.map (\_ subMsg -> subMsg)) rest of
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


pureCmd : msg -> Cmd msg
pureCmd msg =
    Task.succeed msg |> Task.perform identity
