module Views.SongsSummaryList exposing (songsSummaryList)

import Html exposing (Html, text, div, h1, h2, h3, img, input, button, p, span, a)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Keyed as Keyed
import Model exposing (..)
import Types exposing (..)


songRow : Model -> Record Song -> ( String, Html Msg )
songRow model record =
    let
        tags =
            List.map (\t -> span [] [ text t.value.name ]) <|
                tagsFrom model record.id

        linkRoute =
            "#" ++ routeToString (SongPage <| unId record.id)
    in
        ( toString record.id
        , div [ class "songSummaryRow" ]
            [ a [ href linkRoute ] [ h2 [] [ text record.value.name ] ]
            , div [] ([ h3 [] [ text "Tags:" ] ] ++ tags)
            ]
        )


songsSummaryList : Model -> Html Msg
songsSummaryList model =
    div []
        [ Keyed.ul [] <| List.map (songRow model) model.songs ]
