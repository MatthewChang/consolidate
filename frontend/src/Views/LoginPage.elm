module Views.LoginPage exposing (..)

import Html.Styled exposing (..)
import Model exposing (..)
import Types.Input exposing (..)
import Types.Msg exposing (..)
import Views.Components exposing (..)

loginPage : Model -> Html Msg
loginPage model =
    div [] [ textInputWithEnter SubmitPassword PasswordInput model]
