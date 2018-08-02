module Types.KeyOrOtherDropdownOption exposing (..)

import Types exposing (..)

type KeyOrOtherDropdownOption a
    = Existing (Key a)
    | Other
    | None


toValue : KeyOrOtherDropdownOption a -> String
toValue a =
    case a of
        Existing (Key a) ->
            toString a

        Other ->
            "other"

        None ->
            ""
