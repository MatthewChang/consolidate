module Views.NewCardPage exposing (..)

import Html.Keyed as Keyed
import Model exposing (..)
import Types exposing (..)
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Views.Theme exposing (..)


textInput : Model -> InputField -> Html Msg
textInput model ty =
    let
        label =
            inputLabel ty
    in
        div []
            [ div [] [ text <| label ]
            , textarea
                [ placeholder label
                , onInput <| SetInput ty
                , value <| getInputValue model ty 
                , css
                    [ Css.width <| pct 100
                    , Css.height <| px 200
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


newCardPage : Model -> Html Msg
newCardPage model =
    div [ css [ padding <| px 25 ] ]
        [ textInput model NewCardQuestion, textInput model NewCardAnswer]
