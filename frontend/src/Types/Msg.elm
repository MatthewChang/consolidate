module Types.Msg exposing (..)

import Types.Input
import Http
import Navigation
import Time
import Types exposing (..)
import Types.Input exposing (..)
import Ui.Chooser as Chooser

type alias ShowAllResponse = (List (Record Category), List (Record Card))

type Msg
    = HandleUrlChange Navigation.Location
    | InitializeFetch
    | NavigateTo Route
    | ToggleMenu
    | FlipCard (Key Card)
    | PushAlert AlertDialogContents
    | PopAlert
    | SetInput InputField String
    | SetChooser ChooserField Chooser.Msg
    | FetchHomePage (Result Http.Error Int)
    | FetchAllPage (Result Http.Error ShowAllResponse)
    | SubmitNewCardRequest (Result Http.Error Int)
    | GetCategories (Result Http.Error (List (Record Category)))
    | SubmitNewCard

type alias AlertDialogContents =
    { message : String, onConfirm : Msg }
