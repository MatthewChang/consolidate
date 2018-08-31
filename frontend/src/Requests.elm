module Requests exposing (..)

import Decoding exposing (..)
import Encoding exposing (..)
import Types exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (field, int, string, map)
import Http
import Model exposing (..)
import Types.Msg exposing (..)
import Requests.GetAll exposing (..)
import HttpBuilder exposing (..)


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

                EditPage id ->
                    getCard id


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


deleteCard : Key Card -> Cmd Msg
deleteCard key =
    delete ("/cards/" ++ toString (unKey key))
        |> withExpectJson int
        |> send (DeleteCardResponse << Result.map (\x -> key))


getCategories : Cmd Msg
getCategories =
    let
        url =
            "/categories"

        request =
            Http.get url decodeCategories
    in
        Http.send GetCategories request


getCard : Key Card -> Cmd Msg
getCard (Key id) =
    let
        url =
            "/cards/" ++ toString id

        request =
            Http.get url <| Decode.map2 (,) (field "card" decodeCard) (field "categories" decodeCategories)
    in
        Http.send GetCardResponse request


saveCard : NewCardForm -> Key Card -> Cmd Msg
saveCard newCardForm key =
    let
        body =
            Encode.object [ ( "id", encodeKey key ), ( "value", encodeNewCardForm newCardForm ) ]
    in
        put ("/cards/" ++ toString (unKey key))
            |> withExpectJson decodeCard
            |> withJsonBody body
            |> send (DeleteCardResponse << Result.map (\x -> key))
