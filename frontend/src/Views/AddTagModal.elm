module Views.AddTagModal exposing (..)

import Html exposing (Html, text, div, h1, img, input, button, p, select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Html.Keyed as Keyed
import Html.Events.Extra exposing (onEnter)
import Model exposing (..)
import Types exposing (..)
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Views.SongsSummaryList exposing (..)

addTagModal : Model -> Html Msg
addTagModal model =
    let
        options =
            model.tags
                |> List.map
                    (\x ->
                        Dropdown.buttonItem
                            [ onClick <| SelectTag (SelectedTag x) ]
                            [ text x.value.name ]
                    )

        fullOptions =
            [ Dropdown.buttonItem
                [ onClick <| SelectTag Other ]
                [ text "Other" ]
            ]
                ++ options

        ( dropdownText, showOther ) =
            case model.selectedTag of
                None ->
                    ( "Add a tag", False )

                Other ->
                    ( "New", True )

                SelectedTag tag ->
                    ( tag.value.name, False )

        dropDown =
            Dropdown.dropdown
                model.showAddTagDropdown
                { options = []
                , toggleMsg = SetAddTagDropdownState
                , toggleButton =
                    Dropdown.toggle [ Button.primary ] [ text dropdownText ]
                , items = fullOptions
                }

        body =
            if showOther then
                [ dropDown ]
                    ++ [ input
                            [ placeholder "Other"
                            , onInput <| SetInput NewTag
                            , value <| getInputValue model NewTag
                            , onEnter <| AddTag
                            ]
                            []
                       ]
            else
                [ dropDown ]
    in
        div [] body
