module XParser exposing (..)

import AppUtil exposing (decimalSeparator, thousandsSeparator)
import Parser exposing (Parser, (|=), (|.), map, succeed, oneOf, symbol, spaces, backtrackable)

type XValue = XString String 
            | XFloat Float 
            | XEmpty
            | XError String


-- added by Luca following chatGpt
stringParser : Parser XValue
stringParser =
    succeed identity
        |. symbol "\""
        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\"'))
        |. symbol "\""
        |> map XString

-- Custom Float Parser
floatParser : String -> String -> Parser XValue
floatParser thousandsSep decimalSep =
    succeed (\str ->
        str
            |> String.replace thousandsSep ""
            |> String.replace decimalSep "."
            |> String.toFloat
            |> Maybe.withDefault 0.0
            |> XFloat
    )
    |= Parser.getChompedString (Parser.chompWhile (\c -> Char.isDigit c || c == Char.fromCode 44 || c == Char.fromCode 46))

xValueParser : Parser XValue
xValueParser = 
    oneOf 
        [ backtrackable (stringParser )
        , backtrackable (floatParser thousandsSeparator decimalSeparator)  -- Assuming "," as thousands and "." as decimal separator 
        ]

parseOld : String -> XValue
parseOld input =
    case Parser.run xValueParser input of
        Ok xValue ->
            xValue
        Err _ ->
            XError "Error parsing xValue."

parse : String -> Bool -> XValue
parse str forceString =
    if forceString then
        XString str
    else
        let
            -- Function to replace separators and try converting to Float
            tryParseFloat : String -> String -> String -> XValue
            tryParseFloat thousandsSep decimalSep input =
                let
                    cleanedInput =
                        input
                            |> String.replace thousandsSep ""
                            |> String.replace decimalSep "."
                in
                case String.toFloat cleanedInput of
                    Just val ->
                        XFloat val
                    Nothing ->
                        XString input
                        -- XString ("\"" ++ input ++ "\"")
        in
    tryParseFloat thousandsSeparator decimalSeparator str

render : XValue -> String
render xValue =
    case xValue of
        XString s ->
            s
        XFloat f ->
            String.fromFloat f
        XEmpty ->
            ""
        XError e ->
            e