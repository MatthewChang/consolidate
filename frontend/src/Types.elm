module Types exposing (..)

import UrlParser as Url exposing ((</>))
import Http
import Navigation
import Bootstrap.Dropdown as Dropdown
import Time


type Msg
    = UrlChange Navigation.Location
    | InitializeFetch
    | NavigateTo Route
    | SetInput InputField String
    | FetchHomePage (Result Http.Error Int)

type InputField
    = NewTag
    | NewSong



type Id a
    = Id Int


unId : Id a -> Int
unId (Id a) =
    a


type alias JoinEntry a b =
    ( Id a, Id b )


type alias Record a =
    { id : Id a, value : a }


type alias Card =
    { question : String
    , answer : String
    , lastCorrectAt : Float
    , waitDuration : Float
    , categoryId : Id Category
    }


type alias Category =
    { name : String }



--frontend stuff

--routes stuff


type Route
    = RootPage
    | ViewAll


routeParser : Url.Parser (Route -> a) a
routeParser =
    Url.oneOf
        [ Url.map RootPage Url.top
        , Url.map ViewAll (Url.s "all")
        ]


routeToString : Route -> String
routeToString r =
    case r of
        RootPage ->
            "/"

        ViewAll  ->
            "all/"



navigateTo : Route -> Cmd msg
navigateTo a =
    Navigation.newUrl <| "#" ++ (routeToString a)
