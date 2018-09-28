module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Types.Msg exposing (..)
import Navigation
import Subscriptions
import Html.Styled exposing (toUnstyled)


--addTime : (Model, Cmd Msg) -> (Model, Cmd MsgWithTime)
--addTime (m,cmd) = (m,Cmd.map GetTime cmd)

main : Program Never Model Msg
main =
    Navigation.program HandleUrlChange
        { view = view >> toUnstyled
        , init = initialState
        --incase I try the time thing again
        --, update = curry <| addTime << uncurry update
        , update = update
        , subscriptions = Subscriptions.subscriptions
        }
