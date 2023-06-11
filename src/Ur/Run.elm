module Ur.Run exposing (Model, application)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html
import Http
import Json.Decode as JD
import Maybe.Extra
import Task
import Ur exposing (Noun)
import Ur.Constructor as C
import Ur.Deconstructor as D
import Ur.Phonemic
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
    , messageCounter : Int
    , flags : Flags
    , requestsToRetry : List Noun
    }


type Msg msg
    = AppMsg msg
    | EventSourceMsg JD.Value
      -- | ActivateSubscription ( String, String, List String ) Int
    | FailedSubscribe Noun
    | FailedUnsubscribe Noun
    | Noop
    | OpenConnection
    | NeedsActivation


type UrbitAction
    = Subscribe ( String, String, List String )
    | Unsubscribe Int
    | Poke { ship : String, agent : String, mark : String, noun : Noun }


renderUrbitActions : Int -> List ( UrbitAction, a ) -> ( Int, Maybe Noun, List ( Int, a ) )
renderUrbitActions idCounter acts =
    let
        ( newIdCounter, x ) =
            renderUrbitActions_ idCounter acts
    in
    ( newIdCounter
    , if List.isEmpty x then
        Nothing

      else
        x |> List.map Tuple.first |> Maybe.Extra.values |> C.listOf identity |> Just
    , x |> List.map Tuple.second
    )


renderUrbitActions_ : Int -> List ( UrbitAction, a ) -> ( Int, List ( Maybe Noun, ( Int, a ) ) )
renderUrbitActions_ idCounter acts =
    case acts of
        [] ->
            ( idCounter, [] )

        ( act, a ) :: rest ->
            renderUrbitActions_ (idCounter + 1) rest
                |> Tuple.mapSecond
                    (\xs ->
                        ( case act of
                            Subscribe ( ship, app, path ) ->
                                Ur.Phonemic.p ship
                                    |> Maybe.map
                                        (\shipAtom ->
                                            C.cell (C.cord "subscribe") <|
                                                C.cell (C.int idCounter) <|
                                                    C.cell (Ur.Atom shipAtom) <|
                                                        C.cell (C.cord app) (C.listOf C.cord path)
                                        )

                            Unsubscribe subId ->
                                Just <|
                                    C.cell (C.cord "usubscribe") <|
                                        C.cell (C.int idCounter) (C.int subId)

                            Poke { ship, agent, mark, noun } ->
                                Ur.Phonemic.p ship
                                    |> Maybe.map
                                        (\shipAtom ->
                                            C.cell (C.cord "poke") <|
                                                C.cell (C.int idCounter) <|
                                                    C.cell (Ur.Atom shipAtom) <|
                                                        C.cell (C.cord agent) <|
                                                            C.cell (C.cord mark) <|
                                                                noun
                                        )
                        , ( idCounter, a )
                        )
                            :: xs
                    )


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

                    --     urbitSubs_ =
                    --         urbitSubscriptions app |> (\(Ur.Sub.Sub x) -> x)
                    --     urbitSubs =
                    --         urbitSubs_
                    --             |> Dict.map (\_ deconstructor -> { active = Nothing, deconstructor = deconstructor })
                    --     ( messageCounter, actions ) =
                    --         subscriptionActions Dict.empty urbitSubs |> renderUrbitActions 0
                in
                ( { subscriptions = Dict.empty
                  , subscriptionIntMapping = Dict.empty
                  , app = app
                  , connected = False
                  , messageCounter = 0
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


result : (a -> c) -> (b -> c) -> Result a b -> c
result f g res =
    case res of
        Ok b ->
            g b

        Err a ->
            f a


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

                ( messageCounter, newSubscriptionActions, intMapping ) =
                    Dict.diff urbitSubs model.subscriptions
                        |> Dict.toList
                        |> List.map (\( address, _ ) -> ( Subscribe address, address ))
                        |> renderUrbitActions model.messageCounter

                removedSubscriptions =
                    Dict.diff model.subscriptions urbitSubs

                ( messageCounter_, removedSubscriptionActions, _ ) =
                    removedSubscriptions
                        |> Dict.toList
                        |> List.map (\( _, { number } ) -> ( Unsubscribe number, () ))
                        |> renderUrbitActions messageCounter

                foo =
                    intMapping |> List.map (\( a, b ) -> ( b, a )) |> Dict.fromList

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
                        foo
                        urbitSubs
                        Dict.empty
            in
            ( { model
                | app = appModel
                , messageCounter = messageCounter_
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
                , removedSubscriptionActions
                    |> Maybe.map
                        (\noun ->
                            sendUr
                                { noun = noun
                                , url = url
                                , success = Noop
                                , error = FailedUnsubscribe noun
                                }
                        )
                    |> Maybe.withDefault Cmd.none
                , newSubscriptionActions
                    |> Maybe.map
                        (\noun ->
                            sendUr
                                { noun = noun
                                , url = url
                                , success = Noop
                                , error = FailedSubscribe noun
                                }
                        )
                    |> Maybe.withDefault Cmd.none
                ]
            )

        EventSourceMsg value ->
            let
                model_ =
                    model
            in
            case JD.decodeValue (JD.field "message" JD.string) value of
                Ok string ->
                    case D.runBytes (D.cell D.int (D.cell D.cord D.tar) |> D.map (\a b c -> ( a, b, c ) |> Debug.log "event")) (Ur.Uw.decode string) of
                        Just ( _, "watch-ack", _ ) ->
                            -- Not sure what to do. Assume things are fine.
                            ( model_, Cmd.none )

                        Just ( _, "poke-ack", _ ) ->
                            -- Not sure what to do.
                            ( model_, Cmd.none )

                        Just ( subscriptionNumber, "fact", rest ) ->
                            case
                                Dict.get subscriptionNumber model.subscriptionIntMapping |> Maybe.andThen (\key -> Dict.get key model.subscriptions)
                            of
                                Just { deconstructor } ->
                                    case D.run (D.cell D.tar deconstructor |> D.map (\_ subMsg -> subMsg)) rest of
                                        Just subMsg ->
                                            ( model_, pureCmd (AppMsg subMsg) )

                                        -- Got gargbage
                                        Nothing ->
                                            ( model_, Cmd.none )

                                -- Got a fact for a subscription we do not hold
                                Nothing ->
                                    ( model_, Cmd.none )

                        -- got something we don't expect
                        _ ->
                            ( model_, Cmd.none )

                Err _ ->
                    case JD.decodeValue (JD.field "error" JD.value) value of
                        Ok _ ->
                            ( { model_ | connected = False }, Cmd.none )

                        Err _ ->
                            -- we got garbage
                            ( model_, Cmd.none )

        NeedsActivation ->
            let
                ( newMessageCounter, nouns, _ ) =
                    [ ( Poke { ship = "~zod", agent = "hood", mark = "helm-hi", noun = C.cord "Opening airlock!" }, () ) ]
                        |> renderUrbitActions model.messageCounter
            in
            ( { model | messageCounter = newMessageCounter }
            , nouns
                |> Maybe.map (\noun -> sendUr { url = url, noun = noun, success = OpenConnection, error = NeedsActivation })
                |> Maybe.withDefault Cmd.none
            )

        Noop ->
            ( model, Cmd.none )

        FailedSubscribe noun ->
            ( { model | requestsToRetry = noun :: model.requestsToRetry }, Cmd.none )

        FailedUnsubscribe noun ->
            ( { model | requestsToRetry = noun :: model.requestsToRetry }, Cmd.none )

        OpenConnection ->
            ( { model | connected = True }, inp.createEventSource url )


pureCmd : msg -> Cmd msg
pureCmd msg =
    Task.succeed msg |> Task.perform identity


sendUr : { url : String, error : c, success : c, noun : Noun } -> Cmd c
sendUr { url, error, success, noun } =
    Http.riskyRequest
        { method = "PUT"
        , headers = []
        , url = url
        , body = Ur.jam noun |> Ur.Uw.encode |> Http.stringBody "application/x-urb-jam"
        , expect = Http.expectWhatever (result (\_ -> error) (always success))
        , timeout = Nothing
        , tracker = Nothing
        }
