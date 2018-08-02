module Types.Input exposing (..)

import Types.KeyOrOtherDropdownOption exposing (..)
import Types exposing (..)
import Ui.Chooser as Chooser


type InputField
    = NewCardQuestion
    | NewCardAnswer
    | NewCategory


type ChooserField
    = NewCardCategory
    | FilterByCategory


inputLabel : InputField -> String
inputLabel a =
    case a of
        NewCardQuestion ->
            "New Card Question"

        NewCardAnswer ->
            "New Card Answer"

        NewCategory ->
            "Other"


categoryToItem : Record Category -> Chooser.Item
categoryToItem c =
    { label = c.value.name
    , value = toString <| unKey c.id
    , id = toString <| unKey c.id
    }


initNewCardCategoryChooser : List (Record Category) -> Chooser.Model
initNewCardCategoryChooser res =
    let
        catOptions =
            (List.map categoryToItem res)

        options =
            catOptions ++ [{ label = "Other", value = "other", id = "other" }]
    in
        Chooser.init () |> Chooser.updateData options |> Chooser.placeholder "Choose a category"
