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
    }


initialState : Navigation.Location -> ( Model, Cmd Msg )
initialState location =
    ( { history = [ UrlParser.parseHash routeParser location ]
      , inputFields = EveryDict.empty
      , chooserFields = EveryDict.empty
      , requestFinished = False
      , menuOpen = False
      , categories = []
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


getChooserValue : ChooserField -> Model -> Chooser.Model
getChooserValue cf model =
    case EveryDict.get cf model.chooserFields of
        Nothing ->
            Chooser.init () |> Chooser.placeholder "Uninitialized" |> Chooser.closeOnSelect True |> Chooser.searchable True

        Just s ->
            s


setChooserValue : ChooserField -> Chooser.Model -> Model -> Model
setChooserValue field value model =
    { model | chooserFields = EveryDict.insert field value model.chooserFields }
