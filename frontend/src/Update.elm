module Update exposing (..)

import Model exposing (..)
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Navigation
import Types exposing (..)
import Bootstrap.Modal as Modal
import Requests exposing (..)
import Debug
import EveryDict


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTag ->
            ( setInputValue model NewTag "", addTagRequest <| getInputValue model NewTag )

        SubmitNewSong ->
            ( model, submitNewSong <| getInputValue model NewSong )

        SelectTag option ->
            ( { model | selectedTag = option }, Cmd.none )

        SetInput inputType value ->
            ( { model | inputFields = EveryDict.insert inputType value model.inputFields }, Cmd.none )

        FetchHomePage (Ok result) ->
            ( { model
                | songs = result.songs
                , tags = result.tags
                , songTags = result.songTags
                , requestFinished = True
              }
            , Cmd.none
            )

        FetchHomePage (Result.Err _) ->
            ( model, Cmd.none )

        FetchSong (Ok result) ->
            ( model, Cmd.none )

        FetchSong (Result.Err _) ->
            ( model, Cmd.none )

        FetchTag (Ok result) ->
            ( model, Cmd.none )

        FetchTag (Result.Err _) ->
            ( model, Cmd.none )

        FetchTags (Ok result) ->
            ( { model | tags = result }, Cmd.none )

        FetchTags (Result.Err _) ->
            ( model, Cmd.none )

        FetchSongs (Ok result) ->
            ( model, Cmd.none )

        FetchSongs (Result.Err _) ->
            ( model, Cmd.none )

        DeleteSong song ->
            ( model, deleteSong song.id )

        DeleteSongRequest (Ok _) ->
            ( model, navigateTo RootPage )

        DeleteSongRequest (Result.Err _) ->
            ( model, Cmd.none )

        OpenNewSongModal ->
            ( { model | showNewSongModal = Modal.shown }, Cmd.none )

        CloseNewSongModal ->
            ( { model | showNewSongModal = Modal.hidden }, Cmd.none )

        OpenAddTagModal ->
            ( { model | showAddTagModal = Modal.shown }, Cmd.none )

        CloseAddTagModal ->
            ( { model | showAddTagModal = Modal.hidden, selectedTag = None }, Cmd.none )

        SetAddTagDropdownState state ->
            ( { model | showAddTagDropdown = state }, Cmd.none )

        PostSong (Ok song) ->
            let
                newRoute =
                    SongPage <| unId song.id
            in
                ( { model | showNewSongModal = Modal.hidden }, navigateTo newRoute )

        PostSong (Result.Err _) ->
            ( { model | showNewSongModal = Modal.hidden }, Cmd.none )

        PostTag (Ok tag) ->
            ( { model | tags = model.tags ++ [ tag ] }, Cmd.none )

        PostTag (Result.Err _) ->
            ( model, Cmd.none )

        NavigateTo route ->
            ( model, navigateTo route )

        InitializeFetch ->
            case (List.head model.history) of
                Nothing ->
                    ( model, Cmd.none )

                Just route ->
                    ( model, requestForPageLoad route )

        UrlChange location ->
            let
                maybeRoute =
                    Url.parseHash routeParser location
            in
                ( { model | history = maybeRoute :: model.history }, requestForPageLoad maybeRoute )
