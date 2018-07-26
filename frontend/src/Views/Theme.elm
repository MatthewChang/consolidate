module Views.Theme exposing (..)
import Css exposing (..)

theme :
    { primaryDark : Color
    , primaryMiddle : Color
    , primaryLight : Color
    , background : Color
    , text : Color
    , accent : Color
    , backgroundDarker : Color
    }
theme =
    { primaryDark = hex "5680e9"
    , primaryMiddle = hex "5ab9ea"
    , primaryLight = hex "84ceeb"

    --, text = hex "2f2835"
    , text = hex "353535"
    , backgroundDarker = hex "c1c8e4"
    , background = hex "f7f7f7"
    , accent = hex "8860D0"
    }
