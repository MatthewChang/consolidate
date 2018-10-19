module View exposing (..)

import Html.Styled exposing (..)
import Model exposing (..)
import Types exposing (..)
import Views.HomePage exposing (..)
import Views.AppFrame exposing (..)
import Views.NewCardPage exposing (..)
import Views.ViewAllPage exposing (..)
import Views.LoginPage exposing (loginPage)
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
                    viewAllPage model

                Just NewCard ->
                    newCardPage NoEdit model

                Just (EditPage id) ->
                    newCardPage Edit model

                Just LoginPage ->
                    loginPage model
        else
            text "Loading..."
