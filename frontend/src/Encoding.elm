module Encoding exposing (..)

import Json.Decode as Decode exposing (field, int, string)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import Types exposing (..)
import Types.Input exposing (..)
import Model exposing (..)


newCardEncoder : Model -> Encode.Value
newCardEncoder model =
    let
        encode =
            \x ->
                case x of
                    Nothing ->
                        Nothing

                    Just s ->
                        Result.toMaybe <| String.toInt s
    in
        Encode.object
            [ ( "question", Encode.string <| getInputValue CardQuestion model )
            , ( "answer", Encode.string <| getInputValue CardAnswer model )
            , ( "categoryId"
              , maybe Encode.int <| encode <| getChooserValue SelectedCardCategory model
              )
            , ( "newCategory", Encode.string <| getInputValue CategoryOtherInput model )
            ]


encodeNewCardForm : NewCardForm -> Encode.Value
encodeNewCardForm form =
    Encode.object
        [ ( "question", Encode.string <| form.question )
        , ( "answer", Encode.string <| form.answer )
        , ( "categoryId", maybe Encode.int <| Maybe.map unKey form.categoryId )
        , ( "newCategory", Encode.string <| form.newCategory)
        ]


encodeKey : Key a -> Encode.Value
encodeKey (Key k) =
    Encode.int k



--saveCardEncoder :  NewCardForm -> Key Card -> Encode.Value
--saveCardEncoder form key =
--let
--object =
--newCardEncoder model
--editingId =
--Maybe.map (\x -> x.id) model.editingCard
--in
--Encode.object [ ( "id", maybe encodeKey <| editingId ), ( "input", object ) ]
