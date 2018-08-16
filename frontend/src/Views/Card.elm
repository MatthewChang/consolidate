module Views.Card exposing (card)

import Model exposing (..)
import Types exposing (..)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onWithOptions, onClick)
import Views.Theme exposing (..)
import Types.Msg exposing (..)
import Json.Decode as Decode


onClickNoPropagation : Msg -> Html.Styled.Attribute Msg
onClickNoPropagation =
    onWithOptions "click" { preventDefault = True, stopPropagation = True } << Decode.succeed


backView : Record Card -> Html Msg
backView card =
    div []
        [ span [] [ text card.value.answer ]
        , button
            [ onClickNoPropagation <| PushAlert { message = "Are you sure", onConfirm = DeleteCard card.id }
            , css [ float right ]
            ]
            [ text "x" ]
        ]


frontView : Model -> Record Card -> Html Msg
frontView model card =
    let
        flipped =
            getFlipped model card.id

        categoryName =
            case getCategory model card of
                Nothing ->
                    "None?"

                Just c ->
                    c.value.name
    in
        div []
            [ span [] [ text card.value.question ]
            , span [ css [ float right, color theme.accent ] ]
                [ text categoryName ]
            ]


card : Bool -> Model -> Record Card -> Html Msg
card deletable model card =
    let
        cardView =
            if getFlipped model card.id then
                backView card
            else
                frontView model card
    in
        div
            [ css
                [ padding (px 20)
                , backgroundColor theme.backgroundDarker
                , boxShadow5 (px 0) (px 0) (px 20) (px 0) (rgba 0 0 0 0.4)
                , maxWidth (pct 80)
                , color theme.text
                , marginBottom (px 10)
                ]
            , onClick <| FlipCard card.id
            ]
            [ cardView ]
