module Types exposing (..)

import UrlParser as Url exposing ((</>))
import Http
import Navigation
import Bootstrap.Dropdown as Dropdown


type Msg
    = UrlChange Navigation.Location
    | InitializeFetch
    | NavigateTo Route
    | AddTag
    | SubmitNewSong
    | OpenNewSongModal
    | CloseNewSongModal
    | OpenAddTagModal
    | CloseAddTagModal
    | SetAddTagDropdownState Dropdown.State
    | SelectTag SelectTagOption
    | SetInput InputField String
    | DeleteSong (Record Song)
    | DeleteSongRequest (Result Http.Error (Id Song))
    | FetchHomePage (Result Http.Error HomePageResponse)
    | FetchTag (Result Http.Error (Record Song))
    | FetchTags (Result Http.Error (List (Record Tag)))
    | FetchSongs (Result Http.Error (List (Record Song)))
    | FetchSong (Result Http.Error (Record Song))
    | PostTag (Result Http.Error (Record Tag))
    | PostSong (Result Http.Error (Record Song))


type InputField
    = NewTag
    | NewSong


type alias HomePageResponse =
    { songs : List (Record Song), tags : List (Record Tag), songTags : List (JoinEntry Song Tag) }


type Id a
    = Id Int


unId : Id a -> Int
unId (Id a) =
    a


type alias JoinEntry a b =
    ( Id a, Id b )


type alias Record a =
    { id : Id a, value : a }


type alias Tag =
    { name : String }


type alias Song =
    { name : String }



--frontend stuff


type SelectTagOption
    = SelectedTag (Record Tag)
    | Other
    | None



--routes stuff


type Route
    = RootPage
    | TagPage Int
    | SongPage Int
    | SongsPage


routeParser : Url.Parser (Route -> a) a
routeParser =
    Url.oneOf
        [ Url.map RootPage Url.top
        , Url.map TagPage (Url.s "tags" </> Url.int)
        , Url.map SongPage (Url.s "songs" </> Url.int)
        ]


routeToString : Route -> String
routeToString r =
    case r of
        RootPage ->
            "/"

        TagPage a ->
            "tags/" ++ toString a

        SongPage a ->
            "songs/" ++ toString a

        SongsPage ->
            "songs"


navigateTo : Route -> Cmd msg
navigateTo a =
    Navigation.newUrl <| "#" ++ (routeToString a)
