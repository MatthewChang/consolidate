module Decoding exposing (..)

import Json.Decode as Decode exposing (field, int, string, nullable, decodeString)
import Json.Encode as Encode
import Json.Decode.Extra exposing (optionalField)
import Types exposing (..)
import Types.Input exposing (..)
import Model exposing (..)
import Time.Iso8601 exposing (..)
import Time.DateTime exposing (DateTime)


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

decodeReadyCard : Decode.Decoder ( Maybe (Record Card), List (Record Category) )
decodeReadyCard =
    Decode.map2 (,) (field "card" <| nullable decodeCard) (field "categories" decodeCategories)


decodeCardAndCategories : Decode.Decoder ( Record Card, List (Record Category) )
decodeCardAndCategories =
    Decode.map2 (,) (field "card" decodeCard) (field "categories" decodeCategories)


decodeKey : Decode.Decoder (Key a)
decodeKey =
    Decode.map Key int


decodeCategories : Decode.Decoder (List (Record Category))
decodeCategories =
    Decode.list <|
        Decode.map2 (\id name -> Record (Key id) (Category name))
            (field "id" int)
            (field "name" string)

decodeTime : Decode.Decoder DateTime
decodeTime = string  |> Decode.andThen (\x ->
        case toDateTime x of
          Err _ -> Decode.fail "date parse failed"
          Ok time -> Decode.succeed <| time)

decodeCard : Decode.Decoder (Record Card)
decodeCard =
    Decode.map6 (\id question answer dueAt laa categoryId -> Record (Key id) (Card question answer dueAt laa (Key categoryId))) (field "id" int)
        (field "question" string)
        (field "answer" string)
        (field "dueAt" decodeTime)
        (field "lastAnsweredAt" decodeTime)
        (field "categoryId" int)


decodeCards : Decode.Decoder (List (Record Card))
decodeCards =
    Decode.list <| decodeCard
