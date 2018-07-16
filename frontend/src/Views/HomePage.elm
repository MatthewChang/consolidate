module Views.HomePage exposing (..)

import Html exposing (Html, text, div, h1, img, input, button, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Keyed as Keyed
import Model exposing (..)
import Types exposing (..)
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button
import Views.SongsSummaryList exposing (..)


homePage : Model -> Html Msg
homePage model =
    div []
        [ songsSummaryList model
        , button [ onClick OpenNewSongModal ] [ text "New Song" ]
        , Modal.config CloseNewSongModal
            |> Modal.small
            |> Modal.hideOnBackdropClick True
            |> Modal.h3 [] [ text "Modal header" ]
            |> Modal.body [] [ input [ placeholder "New Song", onInput <| SetInput NewSong, value <| getInputValue model NewSong ] [] ]
            |> Modal.footer []
                [ Button.button
                    [ Button.outlinePrimary
                    , Button.attrs [ onClick SubmitNewSong ]
                    ]
                    [ text "Ok" ]
                , Button.button
                    [ Button.outlinePrimary
                    , Button.attrs [ onClick CloseNewSongModal ]
                    ]
                    [ text "Close" ]
                ]
            |> Modal.view model.showNewSongModal
        ]
