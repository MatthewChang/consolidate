module Types exposing (..)

import UrlParser as Url exposing ((</>))
import Navigation
import Time.DateTime exposing (DateTime)


type Key a
    = Key Int


unKey : Key a -> Int
unKey (Key a) =
    a


type alias JoinEntry a b =
    ( Key a, Key b )


type alias Record a =
    { id : Key a, value : a }


type alias Card =
    { question : String
    , answer : String
    , dueAt : DateTime
    , lastAnsweredAt : DateTime
    , categoryId : Key Category
    }


type alias Category =
    { name : String }



--routes stuff


type Route
    = RootPage
    | ViewAll
    | NewCard
    | EditPage (Key Card)
    | LoginPage


routeParser : Url.Parser (Route -> a) a
routeParser =
    Url.oneOf
        [ Url.map RootPage Url.top
        , Url.map ViewAll (Url.s "all")
        , Url.map NewCard (Url.s "new")
        , Url.map LoginPage (Url.s "login")
        , Url.map (EditPage << Key) (Url.s "edit" </> Url.int)
        ]


routeToString : Route -> String
routeToString r =
    case r of
        RootPage ->
            "/"

        ViewAll ->
            "all/"


        NewCard ->
            "new/"

        EditPage (Key id) ->
            "edit/" ++ toString id

        LoginPage ->
            "login/"


navigateTo : Route -> Cmd msg
navigateTo a =
    Navigation.newUrl <| "#" ++ (routeToString a)
