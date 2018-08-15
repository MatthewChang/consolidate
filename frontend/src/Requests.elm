module Requests exposing (..)

import Decoding exposing (..)
import Types exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (field, int, string, map)
import Http
import Model exposing (..)
import Types.Msg exposing (..)
import Requests.GetAll exposing (..)


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
                    getAll

                NewCard ->
                    getCategories


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


getCategories : Cmd Msg
getCategories =
    let
        url =
            "/categories"

        request =
            Http.get url decodeCategories
    in
        Http.send GetCategories request
