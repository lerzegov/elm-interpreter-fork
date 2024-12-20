module Kernel exposing (EvalFunction, functions)

import Array exposing (Array)
import Bitwise
import Core.Array
import Core.Basics
import Core.Bitwise
import Core.Char
import Core.Debug
import Core.Elm.JsArray
import Core.List
import Core.String
import Elm.Syntax.Expression as Expression exposing (Expression(..), FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Environment
import EvalResult
import FastDict as Dict exposing (Dict)
import Kernel.Debug
import Kernel.JsArray
import Kernel.String
import Kernel.Utils
import Maybe.Extra
import Syntax exposing (fakeNode)
import Types exposing (Eval, EvalErrorData, EvalResult, Value(..))
import Value exposing (typeError)
import XModel
import TypesXModel


type alias EvalFunction =
    List Value
    -> List (Node Pattern)
    -> Maybe QualifiedNameRef
    -> Node Expression
    -> Eval Value

-- creates a dict of functions by module wrapping evalFunction
-- in Kernel functions that take func as argument (e.g. foldl filter)
functions : EvalFunction -> Dict ModuleName (Dict String ( Int, List Value -> Eval Value ))
functions evalFunction =
    [ -- Elm.Kernel.Basics
      ( [ "Elm", "Kernel", "Basics" ]
      , [ ( "acos", oneNumber acos round toFloat identity Core.Basics.acos )
        , ( "add", twoNumbers (+) (+) Core.Basics.add )
        , ( "and", two bool bool to bool (&&) Core.Basics.and )
        , ( "asin", oneNumber asin round toFloat identity Core.Basics.asin )
        , ( "atan", oneNumber atan round toFloat identity Core.Basics.atan )
        , ( "atan2", two float float to float atan2 Core.Basics.atan2 )
        , ( "ceiling", oneNumber (ceiling>>toFloat) ceiling toFloat identity Core.Basics.ceiling )
        , ( "cos", oneNumber cos round toFloat identity Core.Basics.cos )
        , ( "e", constant float e )
        , ( "fdiv", twoNumbers (//) (/) Core.Basics.fdiv )
        , ( "floor", oneNumber identity floor toFloat identity Core.Basics.floor )
        , ( "idiv", two int int to int (//) Core.Basics.idiv )
        , ( "isInfinite", one float to bool isInfinite Core.Basics.isInfinite )
        , ( "isNaN", one float to bool isNaN Core.Basics.isNaN )
        , ( "log", oneNumber (logBase e) round toFloat identity log )
        , ( "modBy", two int int to int modBy Core.Basics.modBy )
        , ( "mul", twoNumbers (*) (*) Core.Basics.mul )
        , ( "not", one bool to bool not Core.Basics.not )
        , ( "or", two bool bool to bool (||) Core.Basics.or )
        , ( "pi", constant float pi )
        , ( "pow", twoNumbers (^) (^) Core.Basics.pow )
        , ( "remainderBy", two int int to int remainderBy Core.Basics.remainderBy )
        , ( "round", oneNumber (round >> toFloat) round toFloat identity Core.Basics.round )
        , ( "sin", oneNumber sin round toFloat identity Core.Basics.sin )
        , ( "sqrt", oneNumber sqrt round toFloat identity Core.Basics.sqrt )
        , ( "sub", twoNumbers (-) (-) Core.Basics.sub )
        , ( "tan", oneNumber tan round toFloat identity Core.Basics.tan )
        , ( "toFloat", oneNumber identity round toFloat identity Core.Basics.toFloat )
        , ( "truncate", oneNumber (truncate>>toFloat) truncate toFloat identity Core.Basics.truncate )
        , ( "xor", two bool bool to bool xor Core.Basics.xor )
        ]
      )

    -- Elm.Kernel.Bitwise
    , ( [ "Elm", "Kernel", "Bitwise" ]
      , [ ( "and", two int int to int Bitwise.and Core.Bitwise.and )
        , ( "complement", one int to int Bitwise.complement Core.Bitwise.complement )
        , ( "or", two int int to int Bitwise.or Core.Bitwise.or )
        , ( "shiftLeftBy", two int int to int Bitwise.shiftLeftBy Core.Bitwise.shiftLeftBy )
        , ( "shiftRightBy", two int int to int Bitwise.shiftRightBy Core.Bitwise.shiftRightBy )
        , ( "shiftRightZfBy", two int int to int Bitwise.shiftRightZfBy Core.Bitwise.shiftRightZfBy )
        , ( "xor", two int int to int Bitwise.xor Core.Bitwise.xor )
        ]
      )

    -- Elm.Kernel.Char
    , ( [ "Elm", "Kernel", "Char" ]
      , [ ( "fromCode", one int to char Char.fromCode Core.Char.fromCode )
        , ( "toCode", one char to int Char.toCode Core.Char.toCode )
        , ( "toLocaleLower", one char to char Char.toLocaleLower Core.Char.toLocaleLower )
        , ( "toLocaleUpper", one char to char Char.toLocaleUpper Core.Char.toLocaleUpper )
        , ( "toLower", one char to char Char.toLower Core.Char.toLower )
        , ( "toUpper", one char to char Char.toUpper Core.Char.toUpper )
        ]
      )

    -- Elm.Kernel.Debug
    , ( [ "Elm", "Kernel", "Debug" ]
      , [ ( "log", twoWithError string anything to anything Kernel.Debug.log Core.Debug.log )
        , ( "toString", one anything to string Value.toString Core.Debug.toString )
        , ( "todo", oneWithError string to anything Kernel.Debug.todo Core.Debug.todo )
        ]
      )

    -- Elm.Kernel.JsArray
    , ( [ "Elm", "Kernel", "JsArray" ]
      , [ ( "appendN", three int (jsArray anything) (jsArray anything) to (jsArray anything) Kernel.JsArray.appendN Core.Elm.JsArray.appendN )
        , ( "empty", zero to (jsArray anything) Array.empty )
        , ( "foldr", threeWithError (function2 evalFunction anything anything to anything) anything (jsArray anything) to anything Kernel.JsArray.foldr Core.Elm.JsArray.foldr )
        , ( "foldl", threeWithError (function2 evalFunction anything anything to anything) anything (jsArray anything) to anything Kernel.JsArray.foldl Core.Elm.JsArray.foldl )
        , ( "initialize", threeWithError int int (function evalFunction int to anything) to (jsArray anything) Kernel.JsArray.initialize Core.Elm.JsArray.initialize )
        , ( "initializeFromList", two int (list anything) to (tuple (jsArray anything) (list anything)) Kernel.JsArray.initializeFromList Core.Elm.JsArray.initializeFromList )
        , ( "length", one (jsArray anything) to int Array.length Core.Elm.JsArray.length )
        , ( "map", twoWithError (function evalFunction anything to anything) (jsArray anything) to (jsArray anything) Kernel.JsArray.map Core.Elm.JsArray.map )
        , ( "indexedMap", twoWithError (function2 evalFunction int anything to anything) (jsArray anything) to (jsArray anything) Kernel.JsArray.indexedMap Core.Elm.JsArray.indexedMap )
        , ( "push", two anything (jsArray anything) to (jsArray anything) Array.push Core.Elm.JsArray.push )
        , ( "slice", three int int (jsArray anything) to (jsArray anything) Array.slice Core.Elm.JsArray.slice )
        , ( "singleton", one anything to (jsArray anything) (List.singleton >> Array.fromList) Core.Elm.JsArray.singleton )
        , ( "unsafeGet", twoWithError int (jsArray anything) to anything Kernel.JsArray.unsafeGet Core.Elm.JsArray.unsafeGet )
        , ( "unsafeSet", three int anything (jsArray anything) to (jsArray anything) Array.set Core.Elm.JsArray.unsafeSet )
        ]
      )

    -- Elm.Kernel.List
    , ( [ "Elm", "Kernel", "List" ]
      , [ ( "cons", two anything (list anything) to (list anything) (::) Core.List.cons )
        , ( "fromArray", one (jsArray anything) to (list anything) Array.toList Core.Array.toList )
        , ( "toArray", one (list anything) to (jsArray anything) Array.fromList Core.Array.fromList )
        ]
      )

    -- Elm.Kernel.String
    , ( [ "Elm", "Kernel", "String" ]
      , [ ( "length", one string to int String.length Core.String.length )
        , ( "toFloat", one string to (maybe float) String.toFloat Core.String.toFloat )
        , ( "toInt", one string to (maybe int) String.toInt Core.String.toInt )
        , ( "toLower", one string to string String.toLower Core.String.toLower )
        , ( "toUpper", one string to string String.toUpper Core.String.toUpper )
        , ( "append", two string string to string String.append Core.String.append )
        , ( "cons", two char string to string String.cons Core.String.cons )
        , ( "contains", two string string to bool String.contains Core.String.contains )
        , ( "endsWith", two string string to bool String.endsWith Core.String.endsWith )
        , ( "filter", twoWithError (function evalFunction char to bool) string to string Kernel.String.filter Core.String.filter )
        , ( "foldl", threeWithError (function2 evalFunction char anything to anything) anything string to anything Kernel.String.foldl Core.String.foldl )
        , ( "foldr", threeWithError (function2 evalFunction char anything to anything) anything string to anything Kernel.String.foldr Core.String.foldr )
        , ( "fromList", one (list char) to string String.fromList Core.String.fromList )
        , ( "fromNumber", oneWithError anything to string Kernel.String.fromNumber Core.String.fromFloat ) -- TODO: `fromFloat` is not the same as `fromNumber`
        , ( "indexes", two string string to (list int) String.indexes Core.String.indexes )
        , ( "join", two string (jsArray string) to string (\s a -> String.join s (Array.toList a)) Core.String.join )
        , ( "lines", one string to (list string) String.lines Core.String.lines )
        , ( "reverse", one string to string String.reverse Core.String.reverse )
        , ( "slice", three int int string to string String.slice Core.String.slice )
        , ( "split", two string string to (jsArray string) (\s l -> Array.fromList (String.split s l)) Core.String.split )
        , ( "startsWith", two string string to bool String.startsWith Core.String.startsWith )
        , ( "trim", one string to string String.trim Core.String.trim )
        , ( "trimLeft", one string to string String.trimLeft Core.String.trimLeft )
        , ( "trimRight", one string to string String.trimRight Core.String.trimRight )
        , ( "uncons", one string to (maybe (tuple char string)) String.uncons Core.String.uncons )
        , ( "words", one string to (list string) String.words Core.String.words )
        ]
      )

    -- Elm.Kernel.Utils
    , ( [ "Elm", "Kernel", "Utils" ]
      , [ ( "append", twoWithError anything anything to anything Kernel.Utils.append Core.Basics.append )
        , ( "ge", Kernel.Utils.comparison [ GT, EQ ] )
        , ( "gt", Kernel.Utils.comparison [ GT ] )
        , ( "le", Kernel.Utils.comparison [ LT, EQ ] )
        , ( "lt", Kernel.Utils.comparison [ LT ] )
        , ( "equal", Kernel.Utils.comparison [ EQ ] )
        , ( "notEqual", Kernel.Utils.comparison [ LT, GT ] )
        , ( "compare", twoWithError anything anything to order Kernel.Utils.compare Core.Basics.compare )
        ]
      )
    ]
        |> List.map -- creates a list of function dict for each module
            (\( moduleName, moduleFunctions ) ->
                ( moduleName
                , moduleFunctions
                    |> List.map (\( k, f ) -> ( k, f moduleName ))
                    |> Dict.fromList
                )
            )
        |> Dict.fromList -- creates a dict of module dicts


log : FunctionImplementation
log =
    { name = fakeNode "log"
    , arguments = [ fakeNode <| VarPattern "$x" ]
    , expression =
        fakeNode <|
            Expression.Application
                [ Core.Basics.logBase.expression
                , fakeNode <| FunctionOrValue [] "e"
                , fakeNode <| FunctionOrValue [] "$x"
                ]
    }



-- Selectors


type alias InSelector a x =
    { x
        | fromValue : Value -> Maybe a
        , name : String
    }


type alias OutSelector a x =
    { x
        | toValue : a -> Value
        , name : String
    }


type alias Selector a =
    InSelector a (OutSelector a {})


type To
    = To


to : To
to =
    To


anything : Selector Value
anything =
    { fromValue = Just
    , toValue = identity
    , name = "anything"
    }


order : Selector Order
order =
    { fromValue = Value.toOrder
    , toValue = Value.fromOrder
    , name = "Order"
    }


string : Selector String
string =
    { fromValue =
        \value ->
            case value of
                String s ->
                    Just s

                _ ->
                    Nothing
    , toValue = String
    , name = "String"
    }


float : Selector Float
float =
    { fromValue =
        \value ->
            case value of
                Float s ->
                    Just s

                Int i ->
                    -- Stuff like "2 / 3" is parsed as (Int 2) / (Int 3)
                    Just (toFloat i)

                _ ->
                    Nothing
    , toValue = Float
    , name = "Float"
    }


int : Selector Int
int =
    { fromValue =
        \value ->
            case value of
                Int s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Int
    , name = "Int"
    }


char : Selector Char
char =
    { fromValue =
        \value ->
            case value of
                Char s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Char
    , name = "Char"
    }


bool : Selector Bool
bool =
    { fromValue =
        \value ->
            case value of
                Bool s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Bool
    , name = "Bool"
    }


maybe : Selector a -> Selector (Maybe a)
maybe selector =
    { fromValue =
        \value ->
            case value of
                Custom ctor args ->
                    case ( ctor.moduleName, ctor.name, args ) of
                        ( [ "Maybe" ], "Nothing", [] ) ->
                            Just Nothing

                        ( [ "Maybe" ], "Just", [ arg ] ) ->
                            Maybe.map Just (selector.fromValue arg)

                        _ ->
                            Nothing

                _ ->
                    Nothing
    , toValue =
        \maybeValue ->
            case maybeValue of
                Nothing ->
                    Custom { moduleName = [ "Maybe" ], name = "Nothing" } []

                Just value ->
                    Custom { moduleName = [ "Maybe" ], name = "Just" } [ selector.toValue value ]
    , name = "Maybe " ++ selector.name
    }


list : Selector a -> Selector (List a)
list selector =
    { fromValue =
        \value ->
            case value of
                List l ->
                    Maybe.Extra.traverse selector.fromValue l

                _ ->
                    Nothing
    , toValue =
        \value ->
            value
                |> List.map selector.toValue
                |> List
    , name = "List " ++ selector.name
    }


jsArray : Selector a -> Selector (Array a)
jsArray selector =
    { fromValue =
        \value ->
            case value of
                JsArray jsa ->
                    jsa
                        |> Array.toList
                        |> Maybe.Extra.traverse selector.fromValue
                        |> Maybe.map Array.fromList

                _ ->
                    Nothing
    , toValue =
        \array ->
            array
                |> Array.map selector.toValue
                |> JsArray
    , name = "JsArray " ++ selector.name
    }


function :
    EvalFunction
    -> OutSelector from xf
    -> To
    -> InSelector to xt
    -> InSelector (from -> Eval to) {}
function evalFunctionWith inSelector _ outSelector =
    let
        fromValue : Value -> Maybe (from -> Eval to)
        fromValue value =
            case value of
                PartiallyApplied localEnv oldArgs patterns maybeName implementation ->
                    Just
                        (\arg cfg _ ->
                            evalFunctionWith (oldArgs ++ [ inSelector.toValue arg ]) patterns maybeName implementation cfg localEnv
                                |> EvalResult.onValue
                                    (\out ->
                                        case outSelector.fromValue out of
                                            Just ov ->
                                                Ok ov

                                            Nothing ->
                                                Err <|
                                                    typeError localEnv <|
                                                        "Could not convert output from "
                                                            ++ Value.toString out
                                                            ++ " to "
                                                            ++ outSelector.name
                                    )
                        )

                _ ->
                    Nothing
    in
    { name = inSelector.name ++ " -> " ++ outSelector.name
    , fromValue = fromValue
    }


function2 :
    EvalFunction
    -> OutSelector a xa
    -> OutSelector b xb
    -> To
    -> InSelector to xt
    -> InSelector (a -> Eval (b -> Eval to)) {}
function2 evalFunction in1Selector in2Selector _ outSelector =
    function evalFunction in1Selector to (function evalFunction in2Selector to outSelector)


tuple : Selector a -> Selector b -> Selector ( a, b )
tuple firstSelector secondSelector =
    { fromValue =
        \value ->
            case value of
                Tuple first second ->
                    Maybe.map2 Tuple.pair (firstSelector.fromValue first) (secondSelector.fromValue second)

                _ ->
                    Nothing
    , toValue =
        \( first, second ) ->
            Tuple (firstSelector.toValue first) (secondSelector.toValue second)
    , name = "( " ++ firstSelector.name ++ ", " ++ secondSelector.name ++ ")"
    }


constant : OutSelector res x -> res -> ModuleName -> ( Int, List Value -> Eval Value )
constant selector const _ =
    ( 0
    , \args _ env ->
        case args of
            [] ->
                EvalResult.succeed <| selector.toValue const

            _ ->
                EvalResult.fail <| typeError env <| "Didn't expect any args"
    )


zero :
    To
    -> OutSelector out ox
    -> out
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
zero _ output f =
    zeroWithError To output (Ok f)


zeroWithError :
    To
    -> OutSelector out ox
    -> Result EvalErrorData out
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
zeroWithError _ output f _ =
    ( 0
    , \args _ env ->
        case args of
            [] ->
                EvalResult.fromResult <| Result.map output.toValue f

            _ ->
                EvalResult.fail <| typeError env <| "Expected zero args, got more"
    )


one :
    InSelector a ax
    -> To
    -> OutSelector out ox
    -> (a -> out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
one firstSelector _ output f =
    oneWithError firstSelector To output (\v _ _ -> EvalResult.succeed (f v))

oneNumber :
    (Float -> Float)
    -> (Float -> Int)
    -> (Int -> Float)
    -> (Int -> Int)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
oneNumber fFloat fFloatToInt fIntToFloat fInt implementation moduleName =
    ( 1
    , \args _ env ->
        case args of
            [ Float lf ] ->
                EvalResult.succeed <| Float (fFloat lf)

            [ Int li ] ->
                EvalResult.succeed <| Int (fInt li)

            [ DataAr ar ] ->
                let
                    dAr = XModel.valueToDataArray (DataAr ar)
                    hasExternalDataset =  case Dict.get "hasExternalDataset" ar of
                            Just (Bool s) -> s
                            _ -> False
                    calcData = Array.map fFloat dAr.data
                    calcDAr = { dAr | data = calcData }
                in
                EvalResult.succeed <| (XModel.dataArrayWithDimsToValue calcDAr hasExternalDataset) 

            [ _ ] ->
                partiallyApply moduleName args implementation

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                EvalResult.fail <| typeError env ("Expected one number or DataArray, got " ++ Debug.toString args)
    )



oneWithError :
    InSelector a xa
    -> To
    -> OutSelector out xo
    -> (a -> Eval out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
oneWithError firstSelector _ output f implementation moduleName =
    ( 1
    , \args cfg env ->
        let
            err : String -> EvalResult value
            err got =
                EvalResult.fail <| typeError env <| "Expected one " ++ firstSelector.name ++ ", got " ++ got
        in
        case args of
            [ arg ] ->
                case firstSelector.fromValue arg of
                    Just s ->
                        f s cfg env
                            |> EvalResult.map output.toValue

                    Nothing ->
                        err (Value.toString arg)

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                err "more"
    )


two :
    InSelector a xa
    -> InSelector b xb
    -> To
    -> OutSelector out xo
    -> (a -> b -> out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
two firstSelector secondSelector _ output f =
    twoWithError firstSelector secondSelector To output (\l r _ _ -> EvalResult.succeed (f l r))


twoWithError :
    InSelector a xa
    -> InSelector b xb
    -> To
    -> OutSelector out xo
    -> (a -> b -> Eval out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
twoWithError firstSelector secondSelector _ output f implementation moduleName =
    ( 2
    , \args cfg env ->
        let
            typeError_ : String -> EvalResult value
            typeError_ msg =
                EvalResult.fail (typeError env msg)
        in
        case args of
            [ firstArg, secondArg ] ->
                case firstSelector.fromValue firstArg of
                    Nothing ->
                        typeError_ <| "Expected the first argument to be " ++ firstSelector.name ++ ", got " ++ Value.toString firstArg

                    Just first ->
                        case secondSelector.fromValue secondArg of
                            Nothing ->
                                typeError_ <| "Expected the second argument to be " ++ secondSelector.name ++ ", got " ++ Value.toString secondArg

                            Just second ->
                                f first second cfg env
                                    |> EvalResult.map output.toValue

            [ _ ] ->
                partiallyApply moduleName args implementation

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                let
                    got : String
                    got =
                        String.join ", " <| List.map Value.toString args
                in
                if firstSelector.name == secondSelector.name then
                    typeError_ <| "Expected two " ++ firstSelector.name ++ "s, got " ++ got

                else
                    typeError_ <| "Expected one " ++ firstSelector.name ++ " and one " ++ secondSelector.name ++ ", got " ++ got
    )


three :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
three firstSelector secondSelector thirdSelector _ output f =
    threeWithError firstSelector secondSelector thirdSelector To output (\l m r _ _ -> EvalResult.succeed (f l m r))


threeWithError :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> Eval out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
threeWithError firstSelector secondSelector thirdSelector _ output f implementation moduleName =
    ( 3
    , \args cfg env ->
        let
            err : String -> EvalResult value
            err got =
                if firstSelector.name == secondSelector.name && secondSelector.name == thirdSelector.name then
                    EvalResult.fail <| typeError env <| "Expected three " ++ firstSelector.name ++ "s, got " ++ got

                else
                    EvalResult.fail <| typeError env <| "Expected one " ++ firstSelector.name ++ ", one " ++ secondSelector.name ++ " and one " ++ thirdSelector.name ++ ", got " ++ got
        in
        case args of
            [ firstArg, secondArg, thirdArg ] ->
                case ( firstSelector.fromValue firstArg, secondSelector.fromValue secondArg, thirdSelector.fromValue thirdArg ) of
                    ( Just first, Just second, Just third ) ->
                        f first second third cfg env
                            |> EvalResult.map output.toValue

                    _ ->
                        err (String.join ", " (List.map Value.toString args))

            [ _, _ ] ->
                partiallyApply moduleName args implementation

            [ _ ] ->
                partiallyApply moduleName args implementation

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                err ("[ " ++ String.join ", " (List.map Value.toString args) ++ " ]")
    )


partiallyApply : ModuleName -> List Value -> FunctionImplementation -> EvalResult Value
partiallyApply moduleName args implementation =
    EvalResult.fromResult <|
        Ok <|
            PartiallyApplied
                (Environment.empty moduleName)
                args
                implementation.arguments
                (Just
                    { moduleName = moduleName
                    , name = Node.value implementation.name
                    }
                )
                implementation.expression


twoNumbersOld :
    (Int -> Int -> Int)
    -> (Float -> Float -> Float)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
twoNumbersOld fInt fFloat implementation moduleName =
    ( 2
    , \args _ env ->
        case args of
            [ Int li, Int ri ] ->
                EvalResult.succeed <| Int (fInt li ri)

            [ Int li, Float rf ] ->
                EvalResult.succeed <| Float (fFloat (toFloat li) rf)

            [ Float lf, Int ri ] ->
                EvalResult.succeed <| Float (fFloat lf (toFloat ri))

            [ Float lf, Float rf ] ->
                EvalResult.succeed <| Float (fFloat lf rf)

            [ _ ] ->
                partiallyApply moduleName args implementation

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                EvalResult.fail <| typeError env "Expected two numbers"
    )

twoNumbers :
    (Int -> Int -> Int)
    -> (Float -> Float -> Float)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
twoNumbers fInt fFloat implementation moduleName =
    ( 2
    , \args _ env ->
        case args of
            [ Int li, Int ri ] ->
                EvalResult.succeed <| Int (fInt li ri)

            [ Int li, Float rf ] ->
                EvalResult.succeed <| Float (fFloat (toFloat li) rf)

            [ Float lf, Int ri ] ->
                EvalResult.succeed <| Float (fFloat lf (toFloat ri))

            [ Float lf, Float rf ] ->
                EvalResult.succeed <| Float (fFloat lf rf)
            
            [DataAr ar, Float fl] ->
                let
                    dAr = XModel.valueToDataArray (DataAr ar)
                    hasExternalDataset =  case Dict.get "hasExternalDataset" ar of
                        Just (Bool s) -> s
                        _ -> False
                    calcData = Array.map (\x -> fFloat x fl) dAr.data
                    calcDAr = { dAr | data = calcData }  

                in
                EvalResult.succeed <| (XModel.dataArrayWithDimsToValue calcDAr hasExternalDataset)

            [Float fl, DataAr ar] ->
                let
                    dAr = XModel.valueToDataArray (DataAr ar)
                    hasExternalDataset =  case Dict.get "hasExternalDataset" ar of
                                            Just (Bool s) -> s
                                            _ -> False
                    calcData = Array.map (\x -> fFloat fl x ) dAr.data
                    calcDAr = { dAr | data = calcData }  
                in
                EvalResult.succeed <| (XModel.dataArrayWithDimsToValue calcDAr hasExternalDataset)
            [DataAr ar, Int ri] ->
                let
                    dAr = XModel.valueToDataArray (DataAr ar)
                    hasExternalDataset =  case Dict.get "hasExternalDataset" ar of
                                            Just (Bool s) -> s
                                            _ -> False
                    intToFloat = toFloat ri
                    calcData = Array.map (\x -> fFloat x intToFloat) dAr.data
                    calcDAr = { dAr | data = calcData }  

                in
                EvalResult.succeed <| (XModel.dataArrayWithDimsToValue calcDAr hasExternalDataset)

            [Int li, DataAr ar] ->
                let
                    dAr = XModel.valueToDataArray (DataAr ar)
                    hasExternalDataset =  case Dict.get "hasExternalDataset" ar of
                                            Just (Bool s) -> s
                                            _ -> False
                    intToFloat = toFloat li
                    calcData = Array.map (\x -> fFloat intToFloat x ) dAr.data
                    calcDAr = { dAr | data = calcData }  
                in
                EvalResult.succeed <| (XModel.dataArrayWithDimsToValue calcDAr hasExternalDataset)
                
            [ DataAr la, DataAr ra ] ->
                let
                    leftDAr = XModel.valueToDataArray (DataAr la)
                    leftHasExternalDataset =  case Dict.get "hasExternalDataset" la of
                                            Just (Bool s) -> s
                                            _ -> False
                    rightDAr = XModel.valueToDataArray (DataAr ra)
                    rightHasExternalDataset =  case Dict.get "hasExternalDataset" ra of
                                                Just (Bool s) -> s
                                                _ -> False
                    (adjLeftDAr, adjRightDAr, adjHasExternalDataset) = 
                        if leftHasExternalDataset && not rightHasExternalDataset then
                            (rightDAr, leftDAr, rightHasExternalDataset)
                        else    
                            (leftDAr, rightDAr, leftHasExternalDataset)
                    
                    calcDAr =                         
                            XModel.funcDataArrayPairAr adjLeftDAr adjRightDAr  fFloat
                                |> Tuple.first |> Maybe.withDefault TypesXModel.emptyDataArray

                in
                EvalResult.succeed <| (XModel.dataArrayWithDimsToValue calcDAr adjHasExternalDataset)

            [ _ ] ->
                partiallyApply moduleName args implementation

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                EvalResult.fail <| typeError env ("Expected two numbers" ++ Debug.toString args)
    )
