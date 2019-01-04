module Subscriptions exposing (..)

import Types exposing (..)
import Model exposing (..)
import Bootstrap.Dropdown as Dropdown
import Types.Msg exposing (..)
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second UpdateCurrentTime]

--subscriptions : Model -> Sub Msg
--subscriptions model =
    --Sub.batch
        --[ Dropdown.subscriptions model.showAddTagDropdown SetAddTagDropdownState ]
