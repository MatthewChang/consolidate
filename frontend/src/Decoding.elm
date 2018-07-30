module Decoding exposing (..)

import Json.Decode as Decode exposing (field, int, string)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import Types exposing (..)
import Model exposing (..)


decodeJoins : String -> String -> Decode.Decoder (List (JoinEntry a b))
decodeJoins id1 id2 =
    Decode.list <|
        Decode.map2 (\sid tid -> ( Key sid, Key tid ))
            (field id1 int)
            (field id2 int)



--decodeHome : Decode.Decoder HomePageResponse
--decodeHome =
--Decode.map3 (\songs tags songTags -> HomePageResponse songs tags songTags)
--(field "songs" decodeSongs)
--(field "tags" decodeTags)
--(field "songTags" <| decodeSongTags)


decodeHome : Decode.Decoder Int
decodeHome =
    int


newCardEncoder : Model -> Encode.Value
newCardEncoder model =
    Encode.object
        [ ( "question", Encode.string <| getInputValue model NewCardQuestion )
        , ( "answer", Encode.string <| getInputValue model NewCardAnswer )
        , ( "categoryId", maybe Encode.int <| Nothing )
        , ( "newCategory", Encode.string <| getInputValue model NewCategory )
        ]


decodeCategories : Decode.Decoder (List (Record Category))
decodeCategories =
    Decode.list <| Decode.map2 (\id name -> Record (Key id) (Category name))
        (field "id" int)
        (field "name" string)
