module Types.Msg exposing (..)

import Types.Input
import Http
import Navigation
import Time
import Types exposing (..)
import Types.Input exposing (..)
import Ui.Chooser as Chooser


type Msg
    = HandleUrlChange Navigation.Location
    | InitializeFetch
    | NavigateTo Route
    | SetInput InputField String
    | SetChooser ChooserField Chooser.Msg
    | FetchHomePage (Result Http.Error Int)
    | SubmitNewCardRequest (Result Http.Error Int)
    | GetCategories (Result Http.Error (List (Record Category)))
    | ToggleMenu
    | SubmitNewCard
