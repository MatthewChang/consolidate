module Model exposing (..)

import Navigation
import Bootstrap.Modal as Modal
import Types exposing (..)
import UrlParser
import Bootstrap.Dropdown as Dropdown
import EveryDict
import Task
import Types.Input exposing (..)
import Types.Msg exposing (..)
import Ui.Chooser as Chooser
import Set as Set


performInitialFetch : Cmd Msg
performInitialFetch =
    Task.succeed InitializeFetch |> Task.perform identity


type alias Model =
    { history : List (Maybe Route)
    , inputFields : EveryDict.EveryDict InputField String
    , chooserFields : EveryDict.EveryDict ChooserField Chooser.Model
    , requestFinished : Bool
    , menuOpen : Bool
    , categories : List (Record Category)
    , cards : List (Record Card)
    , flippedCards : EveryDict.EveryDict (Key Card) Bool
    , alerts : List AlertDialogContents
    }


initialState : Navigation.Location -> ( Model, Cmd Msg )
initialState location =
    ( { history = [ UrlParser.parseHash routeParser location ]
      , inputFields = EveryDict.empty
      , chooserFields = EveryDict.empty
      , requestFinished = False
      , menuOpen = False
      , categories = []
      , cards = []
      , flippedCards = EveryDict.empty
      , alerts = []
      }
    , performInitialFetch
    )


currentPage : Model -> Maybe Route
currentPage model =
    case List.head model.history of
        Nothing ->
            Nothing

        Just a ->
            a


getInputValue : InputField -> Model -> String
getInputValue inputField model =
    case EveryDict.get inputField model.inputFields of
        Nothing ->
            ""

        Just s ->
            s


setInputValue : String -> InputField -> Model -> Model
setInputValue value inputField model =
    { model | inputFields = EveryDict.insert inputField value model.inputFields }


getChooserModel : ChooserField -> Model -> Chooser.Model
getChooserModel cf model =
    case EveryDict.get cf model.chooserFields of
        Nothing ->
            Chooser.init () |> Chooser.placeholder "Uninitialized" |> Chooser.closeOnSelect True |> Chooser.searchable True

        Just s ->
            s


getChooserValue : ChooserField -> Model -> Maybe String
getChooserValue cf =
    let
        selected a =
            List.head <| Set.toList <| a.selected
    in
        selected << getChooserModel cf


setChooserValue : ChooserField -> Chooser.Model -> Model -> Model
setChooserValue field value model =
    { model | chooserFields = EveryDict.insert field value model.chooserFields }


getCategory : Model -> Record Card -> Maybe (Record Category)
getCategory model card =
    List.head <| List.filter (\x -> x.id == card.value.categoryId) model.categories


getFlipped : Model -> Key Card -> Bool
getFlipped model id =
    case EveryDict.get id model.flippedCards of
        Nothing ->
            False

        Just a ->
            a
