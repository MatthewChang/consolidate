module View exposing (..)

import Html.Styled exposing (..)
import Model exposing (..)
import Types exposing (..)
import Views.HomePage exposing (..)
import Views.AppFrame exposing (..)
import Views.NewCardPage exposing (..)
import Types.Msg exposing (..)


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
                    newCardPage model
        else
            text "Loading..."
