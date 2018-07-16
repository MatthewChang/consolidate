module Views.SongPage exposing (..)

import Html exposing (Html, text, div, h1, img, input, button, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)
import Types exposing (..)
import Views.ErrorPage exposing (..)
import Views.AddTagModal exposing (..)


tagsViewList : List Tag -> List (Html Msg)
tagsViewList tags =
    List.map (\x -> span [] [ text x.name ]) tags


songPage : Model -> Html Msg
songPage model =
    case currentSong model of
        Nothing ->
            somethingWentWrong "No current song on songs page"

        Just song ->
            let
                tagsView =
                    tagsViewList <| List.map .value <| tagsFrom model song.id
            in
                div [] <|
                    [ Html.h1 [] [ text song.value.name ]
                    , button [ onClick <| DeleteSong song ] [ text "x" ]
                    , div [] <| [ addTagModal model ] ++ tagsView
                    ]
