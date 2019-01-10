module Subscriptions exposing (..)

import Types exposing (..)
import Model exposing (..)
import Bootstrap.Dropdown as Dropdown
import Types.Msg exposing (..)
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        flashSubs =
            if List.length model.flashes > 0 then
                [ Time.every Time.second (\x -> PopFlash) ]
            else
                []
    in
        Sub.batch <| flashSubs



--subscriptions : Model -> Sub Msg
--subscriptions model =
--Sub.batch
--[ Dropdown.subscriptions model.showAddTagDropdown SetAddTagDropdownState ]
