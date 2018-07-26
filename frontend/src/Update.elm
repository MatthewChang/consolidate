module Update exposing (..)

import Model exposing (..)
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Navigation
import Types exposing (..)
import Bootstrap.Modal as Modal
import Requests exposing (..)
import EveryDict


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }, Cmd.none )

        SetInput inputType value ->
            ( { model | inputFields = EveryDict.insert inputType value model.inputFields }, Cmd.none )

        FetchHomePage (Ok result) ->
            ( { model | requestFinished = True }, Cmd.none )

        FetchHomePage (Result.Err _) ->
            ( { model | requestFinished = True }, Cmd.none )

        ------------routing handling
        NavigateTo route ->
            ( { model | menuOpen = False }, navigateTo route )

        InitializeFetch ->
            case (List.head model.history) of
                Nothing ->
                    ( model, Cmd.none )

                Just route ->
                    ( model, requestForPageLoad route )

        HandleUrlChange location ->
            let
                maybeRoute =
                    Url.parseHash routeParser location
            in
                ( { model | history = maybeRoute :: model.history }, requestForPageLoad maybeRoute )
