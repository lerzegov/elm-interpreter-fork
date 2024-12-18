module AppUtil exposing (..)

import List.Extra as ListExtra
import Regex exposing (Regex, contains)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale, System(..), frenchLocale, spanishLocale, usLocale)
import Task exposing (Task)
import Time exposing (Posix, posixToMillis, now)
import Debug
import Process

import Html
import Html.Attributes exposing (style)
import Element exposing (Element , paragraph, text, el)
import Element.Font as UiFont

checkEndOfString : String -> String -> Bool
checkEndOfString fullString textToMatch =
    let
        strLength = String.length fullString
        matchLength = String.length textToMatch
        lastNChars = String.dropLeft (strLength - matchLength) fullString
    in
    if matchLength > strLength then
        False
    else
        lastNChars == textToMatch    
    
    
getUppercaseLetter : Int -> String
getUppercaseLetter asciiCode =
    let
        lettersCount = 26
        quotient = asciiCode // lettersCount
        remainder = modBy lettersCount asciiCode
        letter = String.fromChar (Char.fromCode (Char.toCode 'A' + remainder))
    in
    String.repeat (quotient + 1) letter

renameListItem :String -> String -> List String -> List String
renameListItem oldName newName oldNames =
    List.map (\old  -> 
        if old == oldName then
            newName
        else
            old
    )   oldNames 

debounce : Float -> msg -> Cmd msg
debounce delay message =
    Process.sleep delay
        |> Task.perform (always message)


logOnlyMessageNone : Bool -> String -> Cmd msg
logOnlyMessageNone doLog message =
    -- This will log only the message
    if doLog then
        Debug.log message Cmd.none

    else
        Cmd.none


logOnlyMessageCmd : Bool -> String -> Cmd msg -> Cmd msg
logOnlyMessageCmd doLog message cmd =
    -- This will log only the message
    if doLog then
        Debug.log message cmd

    else
        Cmd.none

myLog : String -> a -> a
myLog message value =
    Debug.log message value

logIf : Bool -> String -> a -> a
logIf condition message value =
    if condition then
        Debug.log message value
    else
        value


removeTrailingUnderscores : String -> String
removeTrailingUnderscores str =
    let
        -- Helper function to check if a string ends with "__"
        endsWithDoubleUnderscore s =
            String.endsWith "_" s
    in
    -- Loop until the string no longer ends with "__"
    if endsWithDoubleUnderscore str then
        removeTrailingUnderscores (String.dropRight 1 str)
    else
        str

nowStr : Task x String
nowStr = 
    now
    |> Task.map posixToMillis
    |> Task.map String.fromInt


cmdMsg : msg -> Cmd msg
cmdMsg message =
    Task.perform (always message) (Task.succeed ())
-- takes a list of strings and returns a tuple of two lists. The first list contains the element "selected" if it is present in the original list, 
-- and the second list contains the remaining elements in the same order as in the original list:
splitSelected : List String -> String -> (List String, List String)
splitSelected strings selected =
    let
        -- Helper function to partition the list
        partition x acc =
            if x == selected then
                { selectedList = x :: acc.selectedList, remainingList = acc.remainingList }
            else
                { selectedList = acc.selectedList, remainingList = x :: acc.remainingList }

        -- Initial accumulator
        initialAcc = { selectedList = [], remainingList = [] }

        -- Fold over the input list to partition it
        result = List.foldl partition initialAcc strings
    in
    ( List.reverse result.selectedList, List.reverse result.remainingList )
intersect : List comparable -> List comparable -> List comparable
intersect list1 list2 =
    List.filter (\x -> List.member x list2) list1
intersectBool : List comparable -> List comparable -> Bool
intersectBool list1 list2 =
    List.any (\x -> List.member x list2) list1

-- returns position indices of the shuffled elements in the original list
-- e.g. shuffledIndices ["a","b","c","d"] ["c","a","d","b"] => [2,0,3,1]
shuffledIndices : List a -> List a -> List Int
shuffledIndices original shuffled = 
    List.map (\sh -> ListExtra.elemIndex sh original |> Maybe.withDefault -1) shuffled
-- sorts origList by shuffledIndices, i.e. positions set for origList elements in returned shuffled list
shuffleNumbersBy: List Int -> List number -> List number
shuffleNumbersBy shuffledIndicesArg origList =
    List.map (\shIdx -> ListExtra.getAt shIdx origList 
            |> Maybe.withDefault -1) shuffledIndicesArg 

shuffleStringsBy: List Int -> List String -> List String
shuffleStringsBy shuffledIndicesArg origList =
    List.map (\shIdx -> ListExtra.getAt shIdx origList 
            |> Maybe.withDefault "") shuffledIndicesArg 

-- aggiunti da luca per gestire i float formattati
-- per non fare casino lo tengo US
decimalSeparator : String
decimalSeparator = "."

thousandsSeparator : String
thousandsSeparator = ","
-- per non fare casino con formato standard float uso US locale
-- tenere allineato con CellParserExcel.parseWithFormattedFloat
myLocale : Locale
myLocale = { decimals = Exact 1, system = Western, thousandSeparator = thousandsSeparator, decimalSeparator = decimalSeparator, negativePrefix = "−", negativeSuffix = "", positivePrefix = "", positiveSuffix = "", zeroPrefix = "", zeroSuffix = "" }

formatFloat : Float  -> String
formatFloat float =
    format myLocale float

-- Function to check if the string contains letters or other characters
containsLetters : String -> Bool
containsLetters string =
    let
        -- Regex that matches any character that is not a digit, comma, or period
        regex : Regex
        regex =
            Regex.fromString "[^0-9.,]" |> Maybe.withDefault Regex.never
    in
    Regex.contains regex string

-- anche in Array.Extra
unique : List comparable -> List comparable
unique l = 
    let
        incUnique : comparable -> List comparable -> List comparable
        incUnique elem lst = 
            if List.member elem lst then
                lst
            else
                elem :: lst
    in
        List.foldr incUnique [] l
fst : (a, b) -> a
fst (x, _) = x

snd : (a, b) -> b
snd (_, y) = y

-- calculates flatIndex from posVec indices and sizes of the dimensions
cartesianProductIndex : List Int -> List Int -> Int
cartesianProductIndex indices lengths =
    let
        indexHelper (index, length) acc =
            if index < 0 || index >= length then
                -1
            else
                acc * length + index
    in
    List.foldl indexHelper 0 (List.reverse (ListExtra.zip indices lengths))


logToParagraph : String -> String -> Element msg
logToParagraph message title =
    let
        logElement =
             Element.html <| Html.div [ style "white-space" "pre-wrap" ] [ Html.text message ]
    in
    paragraph []
    [ el [UiFont.size 20, UiFont.bold] (text title)
    , logElement
    ]


logToParagraphLineBreak : String -> Element msg
logToParagraphLineBreak message =
    let
        lines =
            String.split "\n" message

        lineElements =
            List.map (\line -> Element.html 
                <| Html.pre [ style "white-space" "pre" ]  
                    [ Html.text line ]) lines
    in
    paragraph []
        lineElements


