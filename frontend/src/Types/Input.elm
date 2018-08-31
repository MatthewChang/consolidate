module Types.Input exposing (..)

import Types.KeyOrOtherDropdownOption exposing (..)
import Types exposing (..)
import Ui.Chooser as Chooser


type InputField
    = CardQuestion
    | CardAnswer
    | CategoryOtherInput


type ChooserField
    = SelectedCardCategory
    | FilterByCategory


inputLabel : InputField -> String
inputLabel a =
    case a of
        CardQuestion ->
            "New Card Question"

        CardAnswer ->
            "New Card Answer"

        CategoryOtherInput ->
            "New Category Name"


categoryToItem : Record Category -> Chooser.Item
categoryToItem c =
    { label = c.value.name
    , value = toString <| unKey c.id
    , id = toString <| unKey c.id
    }


initNewCardCategoryChooser : List (Record Category) -> Chooser.Model
initNewCardCategoryChooser list =
    Chooser.init () |> Chooser.placeholder "Choose a category" |> updateCategoryChooserOptions list


initChooser : ChooserField -> Chooser.Model
initChooser field =
    case field of
        SelectedCardCategory ->
            Chooser.init ()
                |> Chooser.placeholder "Choose a category"
                |> Chooser.closeOnSelect True
                |> Chooser.searchable True

        FilterByCategory ->
            Chooser.init ()


updateCategoryChooserOptions : List (Record Category) -> Chooser.Model -> Chooser.Model
updateCategoryChooserOptions list model =
    let
        catOptions =
            (List.map categoryToItem list)

        options =
            catOptions ++ [ { label = "Other", value = "other", id = "other" } ]
    in
        model |> Chooser.updateData options
