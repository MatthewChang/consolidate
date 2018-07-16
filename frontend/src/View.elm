module View exposing (..)

import Html exposing (Html, text, div, h1, img, input, button)
import Model exposing (..)
import Types exposing (..)
import Views.TagPage exposing (..)
import Views.SongPage exposing (..)
import Views.HomePage exposing (..)


view : Model -> Html Msg
view model =
    if model.requestFinished then
        case (currentPage model) of
            Nothing ->
                homePage model

            Just RootPage ->
                homePage model

            Just (TagPage a) ->
                tagPage model (Id a)

            Just (SongPage a) ->
                songPage model

            Just SongsPage ->
                homePage model
    else
        text "Loading..."
