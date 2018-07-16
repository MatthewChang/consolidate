module Views.ErrorPage exposing (..)

import Html exposing (Html, text, div, h1, img, input, button)
import Types exposing (..)


somethingWentWrong : String -> Html Msg
somethingWentWrong error =
    div [] [ Html.h1 [] [ text <| "Some horrible error: " ++ error ] ]
