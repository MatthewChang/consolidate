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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTime msg ->
            ( model, Task.perform (GotTime msg) Time.now )

        GotTime msg time ->
            update msg { model | currentTime = Just time }

        SubmitNewCard ->
            ( model, Requests.submitNewCard model )

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

        GetCategories (Err _) ->
            ( model, Cmd.none )

        SubmitNewCardRequest (Ok categories) ->
            let
                newModel =
                    List.foldl (setInputValue "") model [ CardQuestion, CardAnswer ]

                setcm =
                    setChooserValue SelectedCardCategory ""
            in
                ( setCategories categories <| setcm <| newModel, Cmd.none )

        SubmitNewCardRequest (Err _) ->
            ( model, Cmd.none )

        FetchAllPage (Ok ( cats, cards )) ->
            ( resetFlippedCards <|
                { model
                    | requestFinished = True
                    , categories = cats
                    , cards = cards
                }
            , Cmd.none
            )

        FetchAllPage (Result.Err _) ->
            ( { model | requestFinished = True }, Cmd.none )

        GetReadyCardsResponse (Ok ( card, categories )) ->
            ( resetFlippedCards <| { model | requestFinished = True, categories = categories, readyCard = card }, Cmd.none )

        GetReadyCardsResponse (Result.Err _) ->
            ( { model | requestFinished = True }, Cmd.none )

        MarkCardResponse (Ok ( card, categories )) ->
            ( resetFlippedCards <| { model | categories = categories, readyCard = card }, Cmd.none )

        MarkCardResponse (Result.Err _) ->
            ( model, Cmd.none )

        DeleteCardResponse (Ok result) ->
            ( popAlert <| { model | cards = List.filter (\x -> x.id /= result) model.cards }, Cmd.none )

        DeleteCardResponse (Result.Err _) ->
            ( model, Cmd.none )

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

        GetCardResponse (Result.Err _) ->
            ( model, Cmd.none )

        SaveCardResponse (Ok card) ->
            ( model, Cmd.none )

        SaveCardResponse (Result.Err _) ->
            ( model, Cmd.none )

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
