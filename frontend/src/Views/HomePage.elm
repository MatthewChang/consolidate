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
import Time exposing (..)


pendingCard : Model -> Html Msg
pendingCard model =
    div [] []


homePage : Model -> Html Msg
homePage model =
    let
        none = span [] [ text "None" ]
        display =
            case model.readyCard of
                Nothing ->
                    none

                Just c ->
                  case model.currentTime of
                  Nothing -> Debug.log "no time?" none
                  Just time ->
                    if (fromTimestamp c.value.dueAt) < inSeconds time then
                        card False model c
                    else
                        pendingCard model
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
