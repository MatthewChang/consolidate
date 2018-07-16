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

                TagPage id ->
                    getHome

                --getTag <| Id id
                SongPage id ->
                    getHome

                --getSong <| Id id
                SongsPage ->
                    getTags


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


getTags : Cmd Msg
getTags =
    let
        url =
            "/tags"

        request =
            Http.get url decodeTags
    in
        Http.send FetchTags request


getSong : Id Song -> Cmd Msg
getSong (Id id) =
    let
        url =
            "/songs/" ++ toString id

        request =
            Http.get url decodeSong
    in
        Http.send FetchSong request


getSongs : Cmd Msg
getSongs =
    let
        url =
            "/songs"

        request =
            Http.get url decodeSongs
    in
        Http.send FetchSongs request


getTag : Id Tag -> Cmd Msg
getTag (Id id) =
    let
        url =
            "/tags/" ++ toString id

        request =
            Http.get url decodeSong
    in
        Http.send FetchTag request


addTagRequest : String -> Cmd Msg
addTagRequest a =
    let
        url =
            "/tags"

        request =
            Http.post url (Http.jsonBody (Encode.object [ ( "name", Encode.string a ) ])) decodeTag
    in
        Http.send PostTag request


submitNewSong : String -> Cmd Msg
submitNewSong name =
    let
        url =
            "/songs"

        request =
            Http.post url (Http.jsonBody (Encode.object [ ( "name", Encode.string name ) ])) decodeSong
    in
        Http.send PostSong request


deleteSong : Id Song -> Cmd Msg
deleteSong (Id id) =
    let
        url =
            "/songs/" ++ toString id

        request =
            Http.request
                { method = "DELETE"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , timeout = Nothing
                , expect = Http.expectJson <| map (\x -> Id x) int
                , withCredentials = True
                }
    in
        Http.send DeleteSongRequest request
