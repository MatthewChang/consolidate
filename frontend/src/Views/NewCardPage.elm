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


type EditMode
    = Edit
    | NoEdit


textInputBase : Bool -> InputField -> Model -> Html Msg
textInputBase area ty model =
    let
        label =
            inputLabel ty

        ( comp, rowHeight ) =
            if area then
                ( textarea, 200 )
            else
                ( input, 40 )
    in
        div []
            [ div [] [ text <| label ]
            , comp
                [ placeholder label
                , onInput <| SetInput ty
                , value <| getInputValue ty model
                , css
                    [ Css.width <| pct 100
                    , Css.height <| px rowHeight
                    , color theme.text
                    , backgroundColor theme.backgroundDarker
                    , border <| px 0
                    , padding <| px 9
                    , borderRadius <| px 8
                    , boxShadow5 (px 0) (px 0) (px 20) (px 0) (rgba 0 0 0 0.4)
                    ]
                ]
                []
            ]


textInput : InputField -> Model -> Html Msg
textInput =
    textInputBase False


textArea : InputField -> Model -> Html Msg
textArea =
    textInputBase True


categorySelect : Model -> Html Msg
categorySelect =
    fromUnstyled << Html.map (SetChooser SelectedCardCategory) << Chooser.view << getChooserModel SelectedCardCategory


styledButton : String -> Msg -> Html Msg
styledButton a m =
    button
        [ onClick m
        , css
            [ padding <| px 7
            , backgroundColor theme.primaryDark
            , color theme.primaryLight
            , boxShadow5 (px 0) (px 0) (px 20) (px 0) (rgba 0 0 0 0.4)
            , borderRadius <| px 10
            ]
        ]
        [ text a ]


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
