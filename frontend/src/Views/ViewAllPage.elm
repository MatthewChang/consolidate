module Views.ViewAllPage exposing (..)

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
import Views.Card exposing (..)
import Time.DateTime exposing (..)


viewAllPage : Model -> Html Msg
viewAllPage model =
    let
        cardList =
            List.map (card True model) <| List.reverse <| List.sortBy (\x -> toTimestamp x.value.lastAnsweredAt) model.cards
    in
        div [ css [ padding <| px 25 ] ] <| cardList

