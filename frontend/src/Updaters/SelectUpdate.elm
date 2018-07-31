module Updaters.SelectUpdate exposing (..)

import Model exposing (..)
import Types exposing (..)


selectUpdate : SelectInput -> SelectModel -> SelectModel
selectUpdate msg model =
    case msg of
        NewCardCategory value ->
            { newCardCategorySelect = Just value }

        FilterByCategory ->
            model
