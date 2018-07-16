module Decoding exposing (..)

import Json.Decode as Decode exposing (field, int, string)
import Types exposing (..)


decodeTag : Decode.Decoder (Record Tag)
decodeTag =
    Decode.map2
        (\id name -> Record (Id id) (Tag name))
        (field "id" int)
        (field "name" string)


decodeTags : Decode.Decoder (List (Record Tag))
decodeTags =
    Decode.list (decodeTag)


decodeSong : Decode.Decoder (Record Song)
decodeSong =
    Decode.map2
        (\id name -> Record (Id id) (Song name))
        (field "id" int)
        (field "name" string)


decodeSongs : Decode.Decoder (List (Record Song))
decodeSongs =
    Decode.list (decodeSong)


decodeSongTags : Decode.Decoder (List (JoinEntry Song Tag))
decodeSongTags =
    decodeJoins "songId" "tagId"


decodeJoins : String -> String -> Decode.Decoder (List (JoinEntry a b))
decodeJoins id1 id2 =
    Decode.list <|
        Decode.map2 (\sid tid -> ( Id sid, Id tid ))
            (field id1 int)
            (field id2 int)


decodeHome : Decode.Decoder HomePageResponse
decodeHome =
    Decode.map3 (\songs tags songTags -> HomePageResponse songs tags songTags)
        (field "songs" decodeSongs)
        (field "tags" decodeTags)
        (field "songTags" <| decodeSongTags)
