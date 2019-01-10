module Types.Msg exposing (..)

import Types.Input
import Http
import Navigation
import Time.DateTime exposing (..)
import Time exposing (..)
import Types exposing (..)
import Types.Input exposing (..)
import Ui.Chooser as Chooser


type alias ShowAllResponse =
    ( List (Record Category), List (Record Card) )



--type MsgWithTime = GetTime Msg | GotTime Msg DateTime


type Msg
    = HandleUrlChange Navigation.Location
    | InitializeFetch
    | NavigateTo Route
    | BypassInitialFetch
    | UpdateCurrentTime Time
    | GetTime Msg
    | GotTime Msg Time
    | ToggleMenu
    | FlipCard (Key Card)
    | PushAlert AlertDialogContents
    | PopAlert
    | PushFlash String
    | PopFlash
    | DeleteCard (Key Card)
    | SubmitNewCard
    | SaveCard
    | SetInput InputField String
    | SetChooser ChooserField Chooser.Msg
    | MarkCardAs Bool
    | SubmitPassword
    | GetReadyCardsResponse (Result Http.Error ( Maybe (Record Card), List (Record Category) ))
    | MarkCardResponse (Result Http.Error ( Maybe (Record Card), List (Record Category) ))
    | FetchAllPage (Result Http.Error ShowAllResponse)
    | SubmitNewCardRequest (Result Http.Error (List (Record Category)))
    | GetCategories (Result Http.Error (List (Record Category)))
    | GetCardResponse (Result Http.Error ( Record Card, List (Record Category) ))
    | DeleteCardResponse (Result Http.Error (Key Card))
    | SaveCardResponse (Result Http.Error (Record Card))
    | LoginResponse (Result Http.Error ())


type alias AlertDialogContents =
    { message : String, onConfirm : Msg }


type alias FlashContents =
    String
