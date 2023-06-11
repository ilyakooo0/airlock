port module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Html exposing (Html)
import Json.Decode as JD
import Ur.Deconstructor as D
import Ur
import Ur.Run
import Ur.Sub


main =
    Ur.Run.application
        { init =
            \_ _ ->
                ( { error = ""
                  }
                , Ur.logIn "http://localhost:8080" "lidlut-tabwed-pillex-ridrup"
                    |> Cmd.map (result (Debug.toString >> Error) (always Noop))
                )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        , createEventSource = createEventSource
        , urbitSubscriptions = \_ -> Ur.Sub.subscribe 
            {
                ship = "~zod", app = "journal",
                path = ["updates"]
                , deconstructor = D.tar |> D.map (always Noop)
            }
        , onEventSourceMsg = onEventSourceMessage
        , onUrlChange = \_ -> Noop
        , onUrlRequest = \_ -> Noop
        , urbitUrl = \_ -> "http://localhost:8080"
        }


type alias Model =
    { error : String
    }


type Msg
    = Noop
    | Error String


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Error err ->
            ( { model | error = err }, Cmd.none )


view : Model -> Document Msg
view model =
    { body =
        [ layout [] (column [ centerX, centerY ] [ el [ alignTop ] (text model.error), text "Hello" ]) ]
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
