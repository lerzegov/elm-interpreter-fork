module FormulaParser exposing (..)module FormulaParser exposing (..)

import Parser exposing (..)
import Char exposing (toUpper, toLower)
import String exposing (uncons, fromChar)

type alias DatasetName =
    String

type alias DataArrayName =
    String

type alias CoordName =
    String

type alias HierarchicalName =
    ( DatasetName, DataArrayName, List CoordName )

datasetSeparator : Parser ()
datasetSeparator =
    symbol "__"

nameSeparator : Parser ()
nameSeparator =
    symbol "_"

capitalizeFirst : String -> String
capitalizeFirst str =
    case uncons str of
        Just (first, rest) ->
            fromChar (toUpper first) ++ rest

        Nothing ->
            str

lowerFirst : String -> String
lowerFirst str =
    case uncons str of
        Just (first, rest) ->
            fromChar (toLower first) ++ rest

        Nothing ->
            str

datasetName : Parser DatasetName
datasetName =
    getChompedString (chompWhile Char.isAlphaNum)
        |> map capitalizeFirst

name : Parser String
name =
    getChompedString (chompWhile Char.isAlphaNum)

names : Parser (List String)
names =
    loop [] nameLoop

nameLoop : List String -> Parser (Step (List String) (List String))
nameLoop namesList =
    oneOf
        [ succeed (\_ n -> Loop (namesList ++ [n]))
            |= nameSeparator
            |= name
        , succeed (Done namesList)
        ]

parseNames : Parser (List String)
parseNames =
    succeed (\first rest -> first :: rest)
        |= name
        |= names

hierarchicalName : Parser HierarchicalName
hierarchicalName =
    succeed (\ds nameList ->
        case nameList of
            [] ->
                (ds, "", [])

            x :: xs ->
                (ds, x, xs)
        )
        |= datasetName
        |. datasetSeparator
        |= parseNames

parseHierarchicalName : String -> Result (List (Parser.DeadEnd)) HierarchicalName
parseHierarchicalName input =
    run hierarchicalName input
