module Views.Components exposing (textInput, textArea, styledButton, textInputWithEnter,passwordInputWithEnter)

import Html.Keyed as Keyed
import Model exposing (..)
import Types exposing (..)
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button
import Css exposing (..)
import Html exposing (map)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (value, placeholder, css, type_)
import Html.Styled.Events exposing (..)
import Views.Theme exposing (..)
import Types.Input exposing (..)
import Types.Msg exposing (..)
import Types.KeyOrOtherDropdownOption exposing (..)
import Ui.Chooser as Chooser
import Json.Decode as Decode


textInputBase : Bool -> List (Attribute Msg) -> InputField -> Model -> Html Msg
textInputBase area attr ty model =
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
                ([ placeholder label
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
                    ++ attr
                )
                []
            ]


textInput : InputField -> Model -> Html Msg
textInput =
    textInputBase False []

passwordInputWithEnter : Msg -> InputField -> Model -> Html Msg
passwordInputWithEnter msg =
    textInputBase False [type_ "password", onEnter msg]


textInputWithEnter : Msg -> InputField -> Model -> Html Msg
textInputWithEnter msg =
    textInputBase False [ onEnter msg ]


textArea : InputField -> Model -> Html Msg
textArea =
    textInputBase True []


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



-- attribute for handling enter in text inputs


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        on "keydown" (Decode.andThen isEnter keyCode)
