module Views.AppFrame exposing (..)

import Html.Keyed as Keyed
import Model exposing (..)
import Types exposing (..)
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Views.Theme exposing (..)
import Types.Input exposing (..)
import Types.Msg exposing (..)
import Ui.Modal as Modal
import Ui.Button as Button


toolbarHeight : Px
toolbarHeight =
    (px 50)


menuIconStyled : List Style -> Html Msg
menuIconStyled style =
    div [ css (style ++ [ marginBottom <| px 6 ]) ] <|
        List.repeat 3
            (div
                [ css
                    [ width (px 35)
                    , height <| px 4
                    , backgroundColor theme.primaryLight
                    , marginTop <| px 6
                    , marginLeft <| px 6
                    , marginRight <| px 6
                    ]
                ]
                []
            )


menu : Model -> Html Msg
menu model =
    let
        menuElement =
            (div
                [ css
                    [ position absolute
                    , top toolbarHeight
                    , backgroundColor theme.primaryDark
                    , left <| px 0
                    , boxShadow5 (px 0) (px 0) (px 20) (px 0) (rgba 0 0 0 0.4)
                    ]
                ]
                [ ul
                    [ css
                        [ listStyleType none
                        , padding (px 10)
                        , marginBottom (px 0)
                        ]
                    ]
                    [ li [ onClick <| NavigateTo RootPage ] [ text "Home" ]
                    , li [ onClick <| NavigateTo NewCard ] [ text "Add new" ]
                    , li [ onClick <| NavigateTo ViewAll ] [ text "View All" ]
                    ]
                ]
            )
    in
        if model.menuOpen then
            menuElement
        else
            div [] []


toolbar : Model -> Html Msg
toolbar model =
    div
        [ css
            [ backgroundColor theme.primaryDark
            , padding (px 5)
            , height toolbarHeight
            , boxShadow5 (px 0) (px 0) (px 20) (px 0) (rgba 0 0 0 0.4)
            , zIndex <| int 1
            ]
        ]
        [ div
            [ css
                [ color theme.primaryLight
                , textAlign center
                , width (pct 100)
                , fontSize (px 28)
                , flexGrow <| num 0
                , flexShrink <| num 0
                ]
            ]
            [ div [ onClick ToggleMenu ] [ menuIconStyled [ position absolute, top <| px 7 ] ]
            , menu model
            , span [] [ text "Consolidate" ]
            ]
        ]


alert : AlertDialogContents -> Html Msg
alert contents =
    let
        modalModel =
            Modal.init |> Modal.open |> Modal.closable False

        okButton =
            Button.model "Yes" "primary" "medium"

        closeButton =
            Button.model "No" "secondary" "medium"

        viewModel =
            { contents = List.map toUnstyled [ text "Are you sure?" ]
            , footer = [ Button.render PopAlert closeButton, Button.render contents.onConfirm okButton ]
            , address = \x -> PopAlert
            , title = "are you sure?"
            }
    in
        fromUnstyled <| Modal.render viewModel modalModel


alerts : Model -> Html Msg
alerts model =
    case List.head model.alerts of
        Nothing ->
            div [] []

        Just a ->
            alert a


flashes : Model -> Html Msg
flashes model =
    case List.head model.flashes of
        Nothing ->
            div [] []

        Just c ->
            div
                [ css
                    [ zIndex (int 2)
                    , position absolute
                    , displayFlex
                    , width (pct 100)
                    , justifyContent center
                    ]
                ]
                [ div
                    [ css
                        [ width <| px 200
                        , color theme.text
                        , backgroundColor theme.accent
                        , border <| px 1
                        , borderColor theme.text
                        , borderStyle solid
                        , borderRadius <| px 11
                        , padding <| px 6
                        , marginTop <| px 17
                        , textAlign center
                        ]
                    ]
                    [ text c ]
                ]


appFrame : Model -> Html Msg -> Html Msg
appFrame model body =
    div [ css [ displayFlex, flexDirection column, height <| vh 100 ] ]
        [ flashes model, alerts model, toolbar model, body ]
