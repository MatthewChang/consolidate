module Requests exposing (..)

import Decoding exposing (..)
import Types exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (field, int, string, map)
import Http
import Task
import Debug


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


performInitialFetch : Cmd Msg
performInitialFetch =
    Task.succeed InitializeFetch |> Task.perform identity


getHome : Cmd Msg
getHome =
    let
        url =
            "/home"

        request =
            Http.get url decodeHome
    in
        Http.send FetchHomePage request

