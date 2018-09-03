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
import Views.Theme exposing (..)
import Views.Card exposing (..)
import Types.Msg exposing (..)


homePage : Model -> Html Msg
homePage model =
    let
        display =
            case model.readyCard of
                Nothing ->
                    span [] [ text "None" ]

                Just c ->
                    card False model c
    in
        div
            [ css
                [ displayFlex
                , justifyContent center
                , alignItems center
                , flexGrow <| num 1
                , backgroundColor theme.background
                , overflow scroll
                ]
            ]
            [ display ]
