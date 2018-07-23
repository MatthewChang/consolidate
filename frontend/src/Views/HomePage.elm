module Views.HomePage exposing (..)

import Html exposing (Html, text, div, h1, img, input, button, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Keyed as Keyed
import Model exposing (..)
import Types exposing (..)
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button


homePage : Model -> Html Msg
homePage model =
    div []
        [ text "test"       ]
