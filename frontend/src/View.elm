module View exposing (..)

import Html.Styled exposing (..)
import Model exposing (..)
import Types exposing (..)
import Views.HomePage exposing (..)
import Views.AppFrame exposing (..)


view : Model -> Html Msg
view model =
    appFrame model <|
        if model.requestFinished then
            case (currentPage model) of
                Nothing ->
                    homePage model

                Just RootPage ->
                    homePage model

                Just ViewAll ->
                    homePage model

                Just NewCard ->
                    homePage model
        else
            text "Loading..."
