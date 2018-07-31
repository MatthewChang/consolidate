module Types exposing (..)

import UrlParser as Url exposing ((</>))
import Http
import Navigation
import Bootstrap.Dropdown as Dropdown
import Time


type Msg
    = HandleUrlChange Navigation.Location
    | InitializeFetch
    | NavigateTo Route
    | SetInput InputField String
    | SetSelect SelectInput
    | FetchHomePage (Result Http.Error Int)
    | SubmitNewCardRequest (Result Http.Error Int)
    | GetCategories (Result Http.Error (List (Record Category)))
    | ToggleMenu
    | SubmitNewCard


type InputField
    = NewCardQuestion
    | NewCardAnswer
    | NewCategory


type SelectInput
    = NewCardCategory NewCardCategorySelect
    | FilterByCategory


type NewCardCategorySelect
    = Existing (Key Category)
    | Other


inputLabel : InputField -> String
inputLabel a =
    case a of
        NewCardQuestion ->
            "New Card Question"

        NewCardAnswer ->
            "New Card Answer"

        NewCategory ->
            "Other"


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
    , lastCorrectAt : Float
    , waitDuration : Float
    , categoryId : Key Category
    }


type alias Category =
    { name : String }



--frontend stuff
--routes stuff


type Route
    = RootPage
    | ViewAll
    | NewCard


routeParser : Url.Parser (Route -> a) a
routeParser =
    Url.oneOf
        [ Url.map RootPage Url.top
        , Url.map ViewAll (Url.s "all")
        , Url.map NewCard (Url.s "new")
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


navigateTo : Route -> Cmd msg
navigateTo a =
    Navigation.newUrl <| "#" ++ (routeToString a)
