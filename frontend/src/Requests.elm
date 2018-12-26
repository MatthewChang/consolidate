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
import Task exposing (..)


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

                LoginPage ->
                    Task.succeed BypassInitialFetch |> Task.perform identity


getHome : Cmd Msg
getHome =
    get ("/cards/ready")
        |> withCredentials
        |> withExpectJson decodeReadyCard
        |> send (GetTime << GetReadyCardsResponse)


submitNewCard : Model -> Cmd Msg
submitNewCard model =
    let
        url =
            "/cards"

        request =
            Http.post url (Http.jsonBody (newCardEncoder model)) decodeCategories
    in
        Http.send SubmitNewCardRequest request


deleteCard : Key Card -> Cmd Msg
deleteCard key =
    delete ("/cards/" ++ toString (unKey key))
        |> withCredentials
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
            Http.get url <| decodeCardAndCategories
    in
        Http.send GetCardResponse request


saveCard : NewCardForm -> Key Card -> Cmd Msg
saveCard newCardForm key =
    put ("/cards/" ++ toString (unKey key))
        |> withCredentials
        |> withExpectJson decodeCard
        |> withJsonBody (encodeNewCardForm newCardForm)
        |> send (DeleteCardResponse << Result.map (\x -> key))


markCardAs : Model -> Bool -> Cmd Msg
markCardAs model bool =
    case model.readyCard of
        Nothing ->
            Cmd.none

        Just card ->
            let
                res =
                    if bool then
                        "/correct"
                    else
                        "/wrong"
            in
                post
                    ("/cards/" ++ toString (unKey card.id) ++ res)
                    |> withExpectJson decodeReadyCard
                    |> send (GetTime << MarkCardResponse)


submitPassword : String -> Cmd Msg
submitPassword pw =
    post "/login/"
        |> withCredentials
        |> withJsonBody
            (Encode.object
                [ ( "username", Encode.string "Matthew" )
                , ( "password", Encode.string pw )
                ]
            )
        |> send (LoginResponse)
