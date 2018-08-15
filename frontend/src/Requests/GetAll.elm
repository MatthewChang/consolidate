module Requests.GetAll exposing (..)

import Types exposing (..)
import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode.Extra exposing (maybe)
import Http
import Model exposing (..)
import Types.Msg exposing (..)
import Decoding exposing (..)


getAll : Cmd Msg
getAll =
    let
        url =
            "/all"

        request =
            Http.get url decodeAll
    in
        Http.send FetchAllPage request


decodeAll : D.Decoder ( List (Record Category), List (Record Card) )
decodeAll = D.map2 (,)
        (field "categories" decodeCategories)
        (field "cards" decodeCards)
