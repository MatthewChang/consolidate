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
import Utility exposing (..)
import Task
import Time
import Http exposing (..)


popAlert : Model -> Model
popAlert model =
    { model | alerts = List.drop 1 model.alerts }


setCategories : List (Record Category) -> Model -> Model
setCategories c m =
    let
        newChooserModel =
            updateCategoryChooserOptions c <| getChooserModel SelectedCardCategory m

        newModel =
            setChooserModel SelectedCardCategory newChooserModel m
    in
        { newModel | categories = c }


resetFlippedCards : Model -> Model
resetFlippedCards model =
    { model | flippedCards = EveryDict.empty }



-- Adds a redirect action to a update result


redirectTo : Route -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
redirectTo route ( model, cmd ) =
    -- losing cmd here
    ( { model | menuOpen = False }, navigateTo route )


handleError : Error -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleError err ( m, c ) =
    case err of
        BadStatus r ->
            if r.status.code == 401 then
                redirectTo LoginPage ( m, c )
            else
                ( m, c )

        _ ->
            ( m, c )



--updateFlashes : Model -> Model
--updateFlashes model =
--let
--newFlashes =
--case model.currentTime of
--Just t ->
--List.filter (\x -> x.time >= t) model.flashes
--Nothing ->
--model.flashes
--in
--{ model | flashes = newFlashes }


addFlash : String -> Model -> Model
addFlash message model =
    { model | flashes = message :: model.flashes }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        defaultError =
            \x -> handleError x ( model, Cmd.none )
    in
        case msg of
            UpdateCurrentTime currentTime ->
                ( model, Cmd.none )

            --( updateFlashes <| { model | currentTime = Just currentTime }, Cmd.none )
            GetTime msg ->
                ( model, Task.perform (GotTime msg) Time.now )

            GotTime msg time ->
                update msg { model | currentTime = Just time }

            SubmitNewCard ->
                ( model, Requests.submitNewCard model )

            SubmitPassword ->
                ( model, Requests.submitPassword <| getInputValue PasswordInput model )

            SaveCard ->
                let
                    cmd =
                        case model.editingCard of
                            Nothing ->
                                Cmd.none

                            Just card ->
                                Requests.saveCard (getNewCardForm model) card.id
                in
                    ( model, cmd )

            ToggleMenu ->
                ( { model | menuOpen = not model.menuOpen }, Cmd.none )

            PushAlert options ->
                ( { model | alerts = options :: model.alerts }, Cmd.none )

            PopAlert ->
                ( popAlert model, Cmd.none )

            PushFlash message ->
                ( addFlash message model, Cmd.none )

            PopFlash ->
                ( { model | flashes = List.take (List.length model.flashes - 1) model.flashes }, Cmd.none )

            MarkCardAs bool ->
                ( model, Requests.markCardAs model bool )

            FlipCard key ->
                let
                    val =
                        getFlipped model key
                in
                    ( { model | flippedCards = EveryDict.insert key (not val) model.flippedCards }, Cmd.none )

            DeleteCard key ->
                ( { model | alerts = List.drop 1 model.alerts }, Requests.deleteCard key )

            SetInput inputType value ->
                ( { model | inputFields = EveryDict.insert inputType value model.inputFields }, Cmd.none )

            SetChooser field msg ->
                let
                    ( newModel, cmd ) =
                        Chooser.update msg <| getChooserModel field model
                in
                    ( setChooserModel field newModel model, Cmd.none )

            ----requests
            GetCategories (Ok result) ->
                let
                    setNewDropdown =
                        setChooserModel SelectedCardCategory <| initNewCardCategoryChooser result
                in
                    ( setNewDropdown { model | requestFinished = True, categories = result }
                    , Cmd.none
                    )

            GetCategories (Err e) ->
                defaultError e

            SubmitNewCardRequest (Ok categories) ->
                let
                    newModel =
                        List.foldl (setInputValue "") model [ CardQuestion, CardAnswer ]

                    setcm =
                        setChooserValue SelectedCardCategory ""
                in
                    ( setCategories categories <| setcm <| newModel, Cmd.none )

            SubmitNewCardRequest (Err e) ->
                defaultError e

            FetchAllPage (Ok ( cats, cards )) ->
                ( resetFlippedCards <|
                    { model
                        | requestFinished = True
                        , categories = cats
                        , cards = cards
                    }
                , Cmd.none
                )

            FetchAllPage (Result.Err e) ->
                handleError e ( { model | requestFinished = True }, Cmd.none )

            GetReadyCardsResponse (Ok ( card, categories )) ->
                ( resetFlippedCards <| { model | requestFinished = True, categories = categories, readyCard = card }, Cmd.none )

            GetReadyCardsResponse (Result.Err e) ->
                handleError e ( { model | requestFinished = True }, Cmd.none )

            MarkCardResponse (Ok ( card, categories )) ->
                ( resetFlippedCards <| { model | categories = categories, readyCard = card }, Cmd.none )

            MarkCardResponse (Result.Err e) ->
                defaultError e

            DeleteCardResponse (Ok result) ->
                ( popAlert <| { model | cards = List.filter (\x -> x.id /= result) model.cards }, Cmd.none )

            DeleteCardResponse (Result.Err e) ->
                defaultError e

            GetCardResponse (Ok ( card, categories )) ->
                let
                    newFields =
                        [ ( card.value.question, CardQuestion ), ( card.value.answer, CardAnswer ) ]

                    newModel =
                        List.foldl (uncurry setInputValue) model newFields

                    setcm =
                        setChooserValue SelectedCardCategory (toString <| unKey <| card.value.categoryId)
                in
                    ( setCategories categories <| setcm <| { newModel | editingCard = Just card, requestFinished = True }, Cmd.none )

            GetCardResponse (Result.Err e) ->
                defaultError e

            SaveCardResponse (Ok card) ->
                ( model, Cmd.none )

            SaveCardResponse (Result.Err e) ->
                defaultError e

            LoginResponse (Ok card) ->
                redirectTo RootPage ( model, Cmd.none )

            LoginResponse (Err e) ->
                handleError e ( addFlash "Login Failed." model, Cmd.none )

            BypassInitialFetch ->
                ( { model | requestFinished = True }, Cmd.none )

            ------------routing handling
            NavigateTo route ->
                redirectTo route ( model, Cmd.none )

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
