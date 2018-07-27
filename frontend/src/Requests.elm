module Requests exposing (..)

import Decoding exposing (..)
import Types exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (field, int, string, map)
import Http
import Model exposing (..)


requestForPageLoad : Maybe Route -> Cmd Msg
requestForPageLoad maybeRoute =
    case maybeRoute of
        Nothing ->
            Cmd.none

        Just route ->
            case route of
                RootPage ->
                    getHome

                ViewAll ->
                    getHome

                NewCard ->
                    getHome




getHome : Cmd Msg
getHome =
    let
        url =
            "/home"

        request =
            Http.get url decodeHome
    in
        Http.send FetchHomePage request


submitNewCard : Model -> Cmd Msg
submitNewCard model =
    let
        url =
            "/cards"

        request =
            Http.post url (Http.jsonBody (newCardEncoder model)) decodeHome
    in
        Http.send SubmitNewCardRequest request


