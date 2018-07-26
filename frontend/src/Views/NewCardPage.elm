module Views.HomePage exposing (..)

import Html.Keyed as Keyed
import Model exposing (..)
import Types exposing (..)
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)

newCardPage : Model -> Html Msg
newCardPage model =
    div []
        []
