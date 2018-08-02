module Subscriptions exposing (..)

import Types exposing (..)
import Model exposing (..)
import Bootstrap.Dropdown as Dropdown
import Types.Msg exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

--subscriptions : Model -> Sub Msg
--subscriptions model =
    --Sub.batch
        --[ Dropdown.subscriptions model.showAddTagDropdown SetAddTagDropdownState ]
