module View exposing (..)

import Html exposing (Html, text, div, h1, img, input, button)
import Model exposing (..)
import Types exposing (..)
import Views.HomePage exposing (..)


view : Model -> Html Msg
view model =
    if model.requestFinished then
        case (currentPage model) of
            Nothing ->
                homePage model

            Just RootPage ->
                homePage model
            _ ->
                homePage model
    else
        text "Loading..."
