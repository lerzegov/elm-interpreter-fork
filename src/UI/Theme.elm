module UI.Theme exposing (box, boxRow, button, column, padding, row, rythm, spacing, style, wrappedRow)

import Element exposing (Attribute, Element, alignTop, el, text)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes


rythm : number
rythm =
    10


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    Element.row (spacing :: attrs) children


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    Element.column (spacing :: attrs) children


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs children =
    Element.wrappedRow (spacing :: attrs) children


button : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button attrs config =
    Input.button (padding :: Border.width 1 :: attrs) config


style : String -> String -> Attribute msg
style key value =
    Element.htmlAttribute <| Html.Attributes.style key value


box : String -> List (Attribute msg) -> List (Element msg) -> Element msg
box label attrs content =
    column
        (padding
            :: Border.width 1
            :: alignTop
            :: attrs
        )
        ((el [ Font.bold ] <|
            text label
         )
            :: content
        )

boxRow : String -> List (Attribute msg) -> List (Element msg) -> Element msg
boxRow label attrs content =
    row
        (padding
            :: Border.width 1
            :: alignTop
            :: attrs
        )
        ((el [ Font.bold ] <|
            text label
         )
            :: content
        )
