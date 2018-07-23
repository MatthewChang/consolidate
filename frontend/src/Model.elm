module Model exposing (..)

import Navigation
import Bootstrap.Modal as Modal
import Requests exposing (..)
import Types exposing (..)
import UrlParser
import Bootstrap.Dropdown as Dropdown
import EveryDict


type alias Model =
    { history : List (Maybe Route)
    , inputFields : EveryDict.EveryDict InputField String
    , requestFinished : Bool
    }


initialState : Navigation.Location -> ( Model, Cmd Msg )
initialState location =
    ( { history = [ UrlParser.parseHash routeParser location ]
      , inputFields = EveryDict.empty
      , requestFinished = False
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

getInputValue : Model -> InputField -> String
getInputValue model inputField =
    case EveryDict.get inputField model.inputFields of
        Nothing ->
            ""

        Just s ->
            s


setInputValue : Model -> InputField -> String -> Model
setInputValue model inputField value =
    { model | inputFields = EveryDict.insert inputField value model.inputFields }
