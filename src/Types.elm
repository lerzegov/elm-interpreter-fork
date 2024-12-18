module Types exposing (CallTree(..), Config, Env, EnvValues, Error(..), Eval, EvalErrorData, EvalErrorKind(..), EvalResult
    , PartialEval, PartialResult, Value(..)
    , FormulaModuleInfo, FormulaFunctionInfo, emptyEnv
    , emptyModuleInfo, emptyFunctionInfo, emptyFunctionImplementation, RecalcState(..))

import Array exposing (Array)
import Elm.Syntax.Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern, QualifiedNameRef)
import FastDict as Dict exposing (Dict)
import Parser exposing (DeadEnd)
import Recursion exposing (Rec)
import Rope exposing (Rope)
import TypesXModel exposing (XModel)
import Syntax exposing (fakeNode)
import Elm.Syntax.Expression as Expression




type alias PartialEval out =
    Config -> Env -> PartialResult out


type alias PartialResult out =
    Rec
        ( Node Expression, Config, Env ) -- `r` is the **r**ecursive type.
        (EvalResult out) -- `t` is the **t**arget type that we are converting to
        (EvalResult out) -- `a` is a type for intermediate parts of the computation
-- from Recursion.Rec
{-| An opaque type representing a recursive computation.
I think it is helpful to think of `Rec` like the `Promise` type in javascript. Simliar to a `Promise`, the
result in a `Rec` value might not be available yet because it needs to recursively evaluated in a separate step.
So instead of directly manipulating the value in a `Rec`, we instead can specify actions to be done with the value
when it is available using `map` and `andThen`.

-}


type alias Eval out =
    Config -> Env -> EvalResult out


type alias EvalResult out =
    ( Result EvalErrorData out
    , Rope CallTree
    , Rope String
    )


type alias Config =
    { trace : Bool
    }


type CallTree
    = CallNode
        { expression : Node Expression
        , result : Result EvalErrorData Value
        , children : Rope CallTree
        , env : Env
        }


type Error
    = ParsingError (List DeadEnd)
    | EvalError EvalErrorData
    | IncompleteCodeError String

-- Ther is not Dict!
type Value
    = String String
    | Int Int
    | Float Float
    | Char Char
    | Bool Bool
    | Unit
    | Tuple Value Value
    | Triple Value Value Value
    | Record (Dict String Value)
    | Custom QualifiedNameRef (List Value)
    | PartiallyApplied Env (List Value) (List (Node Pattern)) (Maybe QualifiedNameRef) (Node Expression)
    -- PartiallyApplied env args lamdaFuncArgs (funcModule , funcName) exprImplementation
    -- lamdaFuncArgs are assigned to args in Expression.LambdaExpression
    | JsArray (Array Value)
    | List (List Value)
    | DataAr (Dict String Value)
    -- placeholder for DataArray stored in the model and handlde by update



type alias Env =
    { currentModule : ModuleName
    -- XModel formulas are stored here with moduleName=[datasetRef], other modules are system functions
    , functions : Dict ModuleName (Dict String FunctionImplementation) 
    -- keys only for modules mapped to datasets, fields are additional settings used only by XModel formulas
    , functionsInFormulas : Dict ModuleName FormulaModuleInfo
    , values : EnvValues 
    , envXModel : Maybe XModel --added by Luca
    , callStack : List QualifiedNameRef
    , msgLine : String
    }

emptyEnv : Env
emptyEnv =
    { currentModule = []
    , functions = Dict.empty
    , functionsInFormulas = Dict.empty
    , values = Dict.empty
    , callStack = []
    , envXModel = Nothing
    , msgLine = ""
    }
type alias FormulaModuleInfo =
    { functionDict : Dict String FormulaFunctionInfo
    , functionCalcOrder : List String
    }

type alias FormulaFunctionInfo =
    { calcOrder : Int
    , recalcState : RecalcState
    }
emptyModuleInfo : FormulaModuleInfo
emptyModuleInfo =
    { functionDict = Dict.empty
    , functionCalcOrder = []
    }

emptyFunctionInfo : FormulaFunctionInfo
emptyFunctionInfo =
    { calcOrder = 0
    , recalcState = RecalcNeeded
    }

emptyFunctionImplementation : FunctionImplementation
emptyFunctionImplementation =
    { name = fakeNode ""
    , arguments = []
    , expression = fakeNode Expression.UnitExpr
    }


type RecalcState = RecalcNeeded | RecalcDone

type alias EnvValues =
    Dict String Value


type alias EvalErrorData =
    { currentModule : ModuleName
    , callStack : List QualifiedNameRef
    , error : EvalErrorKind
    }


type EvalErrorKind
    = TypeError String
    | Unsupported String
    | NameError String
    | Todo String
