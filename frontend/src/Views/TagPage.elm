module Views.TagPage exposing (..)

import Html exposing (Html, text, div, h1, img, input, button)
import Model exposing (..)
import Types exposing (..)


tagPage : Model -> Id Tag -> Html Msg
tagPage model id =
    div []
        [ Html.h1 [] [ text "Songs Tagged" ] ]
