module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Types exposing (..)
import Navigation
import Subscriptions

main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , init = initialState
        , update = update
        , subscriptions = Subscriptions.subscriptions
        }
