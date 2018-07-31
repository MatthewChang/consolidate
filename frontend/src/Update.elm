module Update exposing (..)

import Model exposing (..)
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Navigation
import Types exposing (..)
import Bootstrap.Modal as Modal
import Requests
import EveryDict
import Updaters.SelectUpdate exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitNewCard ->
            ( model, Requests.submitNewCard model )

        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }, Cmd.none )

        SetSelect value ->
            ( { model | selectFields = selectUpdate value model.selectFields }, Cmd.none )

        SetInput inputType value ->
            ( { model | inputFields = EveryDict.insert inputType value model.inputFields }, Cmd.none )

        --SetSelect select value ->
            --( { model | inputFields = EveryDict.insert inputType value model.inputFields }, Cmd.none )

        ----requests
        GetCategories (Ok result) ->
            ( { model | requestFinished = True, categories = result }, Cmd.none )

        GetCategories (Err _) ->
            ( model, Cmd.none )

        SubmitNewCardRequest (Ok result) ->
            ( model, Cmd.none )

        SubmitNewCardRequest (Err _) ->
            ( model, Cmd.none )

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
                    ( model, Requests.requestForPageLoad route )

        HandleUrlChange location ->
            let
                maybeRoute =
                    Url.parseHash routeParser location
            in
                ( { model | history = maybeRoute :: model.history }, Requests.requestForPageLoad maybeRoute )
