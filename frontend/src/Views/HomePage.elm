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
import Types.Msg exposing (..)


card : Html Msg
card =
    div
        [ css
            [ padding (px 20)
            , backgroundColor theme.backgroundDarker
            , boxShadow5 (px 0) (px 0) (px 20) (px 0) (rgba 0 0 0 0.4)
            , maxWidth (pct 80)
            , color theme.text
            ]
        ]
        [ span [] [ text "testalsk dfjalskdfjalskdf jalskdjflaskdfjaltestalskdfjalskdfjalskdfjalskdjflaskd fjalsstestalskdfjalskdfjalskdfjalskdjflaskdfjals" ] ]


homePage : Model -> Html Msg
homePage model =
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
        [ card ]
