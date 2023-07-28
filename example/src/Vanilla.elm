port module Vanilla exposing (main)

import BigInt exposing (BigInt)
import Browser exposing (Document)
import Element exposing (..)
import Element.Border as Border
import Heroicons.Outline
import Json.Decode as JD
import Task
import Time
import Ur
import Ur.Cmd
import Ur.Constructor as C
import Ur.Deconstructor as D
import Ur.Run
import Ur.Sub
import Widget
import Widget.Icon as Icon
import Widget.Material as Material


url : String
url =
    "http://localhost:8080"


main : Ur.Run.Program Model Msg
main =
    Ur.Run.application
        { init =
            \_ _ ->
                ( { error = ""
                  , entries = Nothing
                  , newEntry = ""
                  , shipName = Nothing
                  }
                , Cmd.batch
                    [ Ur.logIn url "lidlut-tabwed-pillex-ridrup"
                        |> Cmd.map (result (Debug.toString >> Error) (always Noop))
                    , Ur.getShipName url |> Cmd.map (result (always Noop) GotShipName)
                    , Ur.scry
                        { url = url
                        , agent = "journal"
                        , path = [ "entries", "all" ]
                        , error = Noop
                        , success =
                            D.cell D.ignore
                                (D.cell (D.const D.cord "jrnl")
                                    (D.list (D.cell D.bigint D.cord))
                                )
                                |> D.map (\( (), ( (), listings ) ) -> GotListings listings)
                        }
                    ]
                    |> Ur.Cmd.cmd
                )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        , createEventSource = createEventSource
        , urbitSubscriptions =
            \{ entries, shipName } ->
                case ( entries, shipName ) of
                    ( Just _, Just ship ) ->
                        Ur.Sub.subscribe
                            { ship = ship
                            , app = "journal"
                            , path = [ "updates" ]
                            , deconstructor = decodeJournalUpdate |> D.map GotUpdate
                            }

                    _ ->
                        Ur.Sub.none
        , onEventSourceMsg = onEventSourceMessage
        , onUrlChange = \_ -> Noop
        , onUrlRequest = \_ -> Noop
        , urbitUrl = \_ -> url
        }


type alias Model =
    { error : String
    , entries : Maybe (List ( BigInt, String ))
    , newEntry : String
    , shipName : Maybe String
    }


type Msg
    = Noop
    | Error String
    | GotListings (List ( BigInt, String ))
    | GotUpdate JournalUpdate
    | UpdateNewEntry String
    | DeleteEntry BigInt
    | AddEntry String
    | RunCmd (Ur.Cmd.Cmd Msg)
    | GotShipName String


update : Msg -> Model -> ( Model, Ur.Cmd.Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Ur.Cmd.none )

        Error err ->
            ( { model | error = err }, Ur.Cmd.none )

        GotListings entries ->
            ( { model | entries = Just entries }, Ur.Cmd.none )

        GotUpdate action ->
            let
                applyAction oldEntries =
                    case action of
                        Add id txt ->
                            ( id, txt ) :: oldEntries

                        Edit id txt ->
                            oldEntries
                                |> List.map
                                    (\(( key, _ ) as value) ->
                                        if key == id then
                                            ( key, txt )

                                        else
                                            value
                                    )

                        Delete id ->
                            oldEntries |> List.filter (\( key, _ ) -> key /= id)

                newEntries =
                    model.entries |> Maybe.map applyAction
            in
            ( { model | entries = newEntries }, Ur.Cmd.none )

        UpdateNewEntry txt ->
            ( { model | newEntry = txt }, Ur.Cmd.none )

        DeleteEntry id ->
            ( model
            , Ur.Cmd.poke
                { ship = "~zod"
                , agent = "journal"
                , mark = "journal-action"
                , noun = C.cell (C.cord "del") (C.bigint id)
                }
            )

        AddEntry txt ->
            ( { model | newEntry = "" }
            , Time.now
                |> Task.perform
                    (\time ->
                        Ur.Cmd.poke
                            { ship = "~zod"
                            , agent = "journal"
                            , mark = "journal-action"
                            , noun = C.cell (C.cord "add") (C.cell (time |> Time.posixToMillis |> BigInt.fromInt |> C.bigint) (C.cord txt))
                            }
                            |> RunCmd
                    )
                |> Ur.Cmd.cmd
            )

        RunCmd cmd ->
            ( model, cmd )

        GotShipName name ->
            ( { model | shipName = Just name }, Ur.Cmd.none )


type JournalUpdate
    = Add BigInt String
    | Edit BigInt String
    | Delete BigInt


decodeJournalUpdate : D.Deconstructor JournalUpdate
decodeJournalUpdate =
    D.oneOf
        [ (D.cell (D.const D.cord "add") <| D.cell D.bigint D.cord) |> D.map (\( (), ( id, txt ) ) -> Add id txt)
        , D.cell (D.const D.cord "edit") (D.cell D.bigint D.cord) |> D.map (\( (), ( id, txt ) ) -> Edit id txt)
        , D.cell (D.const D.cord "del") D.bigint |> D.map (\( (), id ) -> Delete id)
        ]
        |> D.cell D.ignore
        |> D.map (\( (), upd ) -> upd)


view : Model -> Document Msg
view model =
    { body =
        [ layout []
            ([ el [ alignTop ] (text model.error)
             , row [ spacing 8 ]
                [ Widget.textInput (Material.textInput Material.defaultPalette)
                    { chips = []
                    , text = model.newEntry
                    , placeholder = Nothing
                    , label = "New entry"
                    , onChange = UpdateNewEntry
                    }
                , Widget.iconButton (Material.containedButton Material.defaultPalette)
                    { text = "submit"
                    , icon = Icon.elmHeroicons Heroicons.Outline.check
                    , onPress = AddEntry model.newEntry |> Just
                    }
                ]
             , model.entries
                |> Maybe.withDefault []
                |> List.map
                    (\( id, txt ) ->
                        row
                            [ Border.rounded 10
                            , Border.shadow { offset = ( 0, 5 ), size = 1, blur = 10, color = rgba 0 0 0 0.3 }
                            , centerX
                            , padding 10
                            , spacing 12
                            ]
                            [ paragraph [] [ text txt ]
                            , Widget.iconButton (Material.containedButton Material.defaultPalette)
                                { text = "delete"
                                , icon = Icon.elmHeroicons Heroicons.Outline.trash
                                , onPress = DeleteEntry id |> Just
                                }
                            ]
                    )
                |> column [ spacing 10, centerX ]
             ]
                |> column [ spacing 18, centerX ]
            )
        ]
    , title = "Airlock"
    }


result : (a -> c) -> (b -> c) -> Result a b -> c
result f g res =
    case res of
        Ok b ->
            g b

        Err a ->
            f a


port createEventSource : String -> Cmd msg


port onEventSourceMessage : (JD.Value -> msg) -> Sub msg
