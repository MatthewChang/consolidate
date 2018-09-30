module Views.HomePage exposing (..)

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
import Views.Card exposing (..)
import Types.Msg exposing (..)
import Time exposing (..)
import Time.DateTime exposing (..)
import Utility exposing (..)


pendingCard : DateTime -> DateTime -> Html Msg
pendingCard currentTime time =
    let
        diff =
            delta time currentTime

        values =
            [ ( diff.days, "days" )
            , ( diff.hours % 24, "hours" )
            , ( diff.minutes % 60, "minutes" )
            , ( diff.seconds % 60, "seconds" )
            ]

        --use scanl to get diffs
        filtered =
            List.take 2 <| List.filter (\( x, label ) -> x > 0 || label == "seconds") (log values)

        combine =
            \( num, l ) str -> str ++ " " ++ toString num ++ " " ++ l

        string =
            List.foldl combine "" filtered
    in
        span [] [ text string ]


homePage : Model -> Html Msg
homePage model =
    let
        display =
            case model.readyCard of
                Nothing ->
                    span [] [ text "None" ]

                Just c ->
                    case Maybe.map fromTimestamp model.currentTime of
                        Nothing ->
                            Debug.log "no time?" <| span [] [ text "No Time Found" ]

                        Just time ->
                            case Time.DateTime.compare c.value.dueAt time of
                                LT ->
                                    card False model c

                                _ ->
                                    pendingCard time c.value.dueAt
    in
        div
            [ css
                [ displayFlex
                , justifyContent center
                , alignItems center
                , flexGrow <| num 1
                , backgroundColor theme.background
                , overflow scroll
                ]
            ]
            [ display ]
