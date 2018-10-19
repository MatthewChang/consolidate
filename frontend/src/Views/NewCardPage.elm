module Views.NewCardPage exposing (..)

import Html.Keyed as Keyed
import Model exposing (..)
import Types exposing (..)
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button
import Css exposing (..)
import Html exposing (map)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (value, placeholder, css)
import Html.Styled.Events exposing (..)
import Views.Theme exposing (..)
import Types.Input exposing (..)
import Types.Msg exposing (..)
import Types.KeyOrOtherDropdownOption exposing (..)
import Ui.Chooser as Chooser
import Views.Components exposing (..)


type EditMode
    = Edit
    | NoEdit

categorySelect : Model -> Html Msg
categorySelect =
    fromUnstyled << Html.map (SetChooser SelectedCardCategory) << Chooser.view << getChooserModel SelectedCardCategory

otherField : Model -> List (Html Msg)
otherField model =
    if getChooserValue SelectedCardCategory model == Just "other" then
        [ textInput CategoryOtherInput model ]
    else
        []


newCardPage : EditMode -> Model -> Html Msg
newCardPage editMode model =
    let
        saveMethod =
            case editMode of
                Edit ->
                    SaveCard

                NoEdit ->
                    SubmitNewCard
    in
        div [ css [ padding <| px 25 ] ] <|
            [ textArea CardQuestion model
            , textArea CardAnswer model
            , categorySelect model
            ]
                ++ otherField model
                ++ [ styledButton "Done" saveMethod ]
