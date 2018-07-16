module Model exposing (..)

import Navigation
import Bootstrap.Modal as Modal
import Requests exposing (..)
import Types exposing (..)
import UrlParser
import Bootstrap.Dropdown as Dropdown
import EveryDict


type alias Model =
    { history : List (Maybe Route)
    , tags : List (Record Tag)
    , songs : List (Record Song)
    , songTags : List (JoinEntry Song Tag)
    , selectedTag : SelectTagOption
    , showNewSongModal : Modal.Visibility
    , showAddTagModal : Modal.Visibility
    , showAddTagDropdown : Dropdown.State
    , inputFields : EveryDict.EveryDict InputField String
    , requestFinished : Bool
    }


initialState : Navigation.Location -> ( Model, Cmd Msg )
initialState location =
    ( { history = [ UrlParser.parseHash routeParser location ]
      , tags = []
      , songs = []
      , songTags = []
      , selectedTag = None
      , showNewSongModal = Modal.hidden
      , showAddTagModal = Modal.hidden
      , showAddTagDropdown = Dropdown.initialState
      , inputFields = EveryDict.empty
      , requestFinished = False
      }
    , performInitialFetch
    )


tagsFrom : Model -> Id Song -> List (Record Tag)
tagsFrom model id =
    let
        joins =
            List.filter (\( songid, _ ) -> songid == id) model.songTags

        ids =
            List.map (\( _, tagid ) -> tagid) joins
    in
        List.filter (\r -> List.member r.id ids) model.tags


currentPage : Model -> Maybe Route
currentPage model =
    case List.head model.history of
        Nothing ->
            Nothing

        Just a ->
            a


getSong : Model -> Id Song -> Maybe (Record Song)
getSong model id =
    List.head (List.filter (\x -> x.id == id) model.songs)


currentSong : Model -> Maybe (Record Song)
currentSong model =
    case currentPage model of
        Just (SongPage a) ->
            getSong model <| Id a

        _ ->
            Nothing


getInputValue : Model -> InputField -> String
getInputValue model inputField =
    case EveryDict.get inputField model.inputFields of
        Nothing ->
            ""

        Just s ->
            s


setInputValue : Model -> InputField -> String -> Model
setInputValue model inputField value =
    { model | inputFields = EveryDict.insert inputField value model.inputFields }
