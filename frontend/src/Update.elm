module Update exposing (..)

import Model exposing (..)
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Navigation
import Types exposing (..)
import Types.Input exposing (..)
import Bootstrap.Modal as Modal
import Requests
import EveryDict
import Ui.Chooser as Chooser
import Types.Msg exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitNewCard ->
            ( model, Requests.submitNewCard model )

        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }, Cmd.none )

        PushAlert options ->
            (  {model | alerts = options :: model.alerts }, Cmd.none )

        PopAlert ->
            (  {model | alerts = List.drop 1 model.alerts }, Cmd.none )

        FlipCard key ->
            let
                val =
                    getFlipped model key
            in
                ( { model | flippedCards = EveryDict.insert key (not val) model.flippedCards }, Cmd.none )

        SetInput inputType value ->
            ( { model | inputFields = EveryDict.insert inputType value model.inputFields }, Cmd.none )

        SetChooser field msg ->
            let
                ( newModel, cmd ) =
                    Chooser.update msg <| getChooserModel field model
            in
                ( setChooserValue field newModel model, Cmd.none )

        ----requests
        GetCategories (Ok result) ->
            let
                setNewDropdown =
                    setChooserValue NewCardCategory <| initNewCardCategoryChooser result
            in
                ( setNewDropdown { model | requestFinished = True, categories = result }
                , Cmd.none
                )

        GetCategories (Err _) ->
            ( model, Cmd.none )

        SubmitNewCardRequest (Ok result) ->
            ( model, Cmd.none )

        SubmitNewCardRequest (Err _) ->
            ( model, Cmd.none )

        FetchAllPage (Ok ( cats, cards )) ->
            ( { model
                | requestFinished = True
                , categories = cats
                , cards = cards
              }
            , Cmd.none
            )

        FetchAllPage (Result.Err _) ->
            ( { model | requestFinished = True }, Cmd.none )

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
