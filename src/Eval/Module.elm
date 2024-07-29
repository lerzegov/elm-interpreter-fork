module Eval.Module exposing (eval, trace, traceOrEvalModule, evalModuleWithEnv
            , makeEnv, buildInitialEnv, emptyEnv, emptyEnvWithCoreFunctions)


import Core
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
-- NB Env is in Types.elm, Environment has functions to add functions and values to the environment
import Environment 
import Eval.Expression
import FastDict as Dict exposing (Dict)
import List.Extra
import Result.MyExtra
import Rope exposing (Rope)
import Syntax exposing (fakeNode)
import Types exposing (CallTree, Env, Error(..), Value(..), FormulaModuleInfo)
import Value exposing (unsupported)
-- used to call createTestEnvValues
import TypesXModel exposing (XModel)
import XModel exposing (myXModel)
import Types exposing (FormulaFunctionInfo, RecalcState(..))




-- used nowhere, UI.elm calls directly traceOrEvalModule
eval : String -> Expression -> Result Error Value
-- source: module source, expression: expression to evaluate (default `main`)
eval source expression =
    let
        ( result, _, _ ) =
            traceOrEvalModule { trace = False } source expression
    in
    result


trace : String -> Expression -> ( Result Error Value, Rope CallTree, Rope String )
trace  source expression =
    traceOrEvalModule  { trace = True } source expression

-- added by Luca => evaluates ONE expression in a module source with a given environment
evalModuleWithEnv : String -> Expression -> Result Error Env -> ( Result Error Value, Rope CallTree, Rope String )
-- source: module source, expression: expression to evaluate (default `main`)
-- used only to get Node coordinates for the name of the expression to evaluate
-- call example in UI.elm: 
--     traceOrEvalModule {trace=False} moduleSource (Expression.FunctionOrValue [] "main")
evalModuleWithEnv source expression maybeEnv =
    -- initialEnv including functions and values from moduleSource is checked for errors
    case maybeEnv of
        Err e ->
            ( Err e, Rope.empty, Rope.empty )
        -- if env is built correctly, scan for computed expression definition (name) in source
        -- NB: source is parsed only to locate the Node for the expression to evaluate
        -- the functions in the environment are not updated with the functions in the source
        Ok env ->
            let
                exprName : String
                exprName =
                    case expression of
                        FunctionOrValue moduleName name ->
                            (Debug.toString moduleName) ++ ":" ++ name
                        _ ->
                            "noName"
                -- function to create Node from expression
                maybeNode : a -> Node a
                maybeNode =
                    case expression of
                        -- expression arg is FunctionOrValue named `name`
                        FunctionOrValue [] name ->
                            let
                                needle : String
                                -- define start of func or value declaration
                                needle =
                                    name ++ " ="
                            in
                            -- find expression in source and create Node for it assigning Range
                            -- Node is base representation for a syntax node in a source file
                            -- identified by a range in the source code 
                            -- (1-based start and end position of defining string)
                            source
                                |> String.split "\n"
                                -- find index of line where function or value is defined
                                |> List.Extra.findIndex
                                    (String.startsWith needle)
                                -- create constructor for Node expression passing start and end position
                                -- of its name in the source code
                                |> Maybe.map
                                    (\index ->
                                        Node -- Node is type defined in Elm.Syntax.Node
                                            -- Range {Location,Location}
                                            { start = { row = index + 1, column = 1 }
                                            , end = { row = index + 1, column = 1 + String.length name }
                                            }
                                    )
                                |> Maybe.withDefault fakeNode

                        _ ->
                            fakeNode
                -- passes expression wrapped in Node to expression evaluator
                -- together with environment env with declared functions and values, and trace flag
                ( result, callTrees, logLines ) =
                    -- expression evaluation
                    Eval.Expression.evalExpression
                        (maybeNode expression) -- expression to evaluate wrapped in Node
                        { trace = False }
                        env -- environment with declared functions and values passed here

            in
            -- Debug.log ("evalModuleWithEnv for expr => " ++ exprName) -- ++ " result => " ++ Debug.toString result)
            ( Result.mapError Types.EvalError result -- returned Result, if Err maps to EvalError
            , callTrees
            , logLines
            )

-- added by Luca
makeEnv : String -> Maybe Env -> Result Error Env
makeEnv source curEnv  =
    source -- module source processing
        |> Elm.Parser.parse
        |> Result.mapError ParsingError -- if parsing fails, map to ParsingError
        |> Result.map -- if parsing is successful, process the source
            (\rawFile ->
                let
                    context : Elm.Processing.ProcessContext
                    context =
                        Elm.Processing.init 
                            |> Elm.Processing.addDependency Core.dependency
                in
                Elm.Processing.process context rawFile
            ) -- ==> returns File !!!
        -- ... then calls buildInitialEnv passing curEnv (with declared functions and values),  passing File!!!
        |> Result.andThen (buildInitialEnv curEnv  ) -- pass processed source with dependencies to buildInitialEnv

traceOrEvalModule : { trace : Bool } -> String -> Expression -> ( Result Error Value, Rope CallTree, Rope String )
-- source: module source, expression: expression to evaluate (default `main`)
-- call example in UI.elm: 
--     traceOrEvalModule {trace=False} moduleSource (Expression.FunctionOrValue [] "main")
traceOrEvalModule cfg source expression =
    let
        -- parse moduleSource and build environment with declared functions and values
        maybeEnv : Result Error Env
        maybeEnv =
            source -- module source processing
                |> Elm.Parser.parse
                |> Result.mapError ParsingError -- if parsing fails, map to ParsingError
                |> Result.map -- if parsing is successful, process the source
                    (\rawFile ->
                        let
                            context : Elm.Processing.ProcessContext
                            context =
                                Elm.Processing.init 
                                    |> Elm.Processing.addDependency Core.dependency
                        in
                        Elm.Processing.process context rawFile
                    )
                -- build env with declared functions and values
                |> Result.andThen (buildInitialEnv Nothing) -- pass processed source with dependencies to buildInitialEnv
    in
    -- initialEnv including functions and values from moduleSource is checked for errors
    case maybeEnv of
        Err e ->
            ( Err e, Rope.empty, Rope.empty )
        -- if env is built correctly, scan for computed expression definition (name) in source
        Ok env ->
            let
                -- function to create Node from expression
                maybeNode : a -> Node a
                maybeNode =
                    case expression of
                        -- expression arg is FunctionOrValue named `name`
                        FunctionOrValue [] name ->
                            let
                                needle : String
                                -- define start of func or value declaration
                                needle =
                                    name ++ " ="
                            in
                            -- find expression in source and create Node for it assigning Range
                            -- Node is base representation for a syntax node in a source file
                            -- identified by a range in the source code 
                            -- (1-based start and end position of defining string)
                            source
                                |> String.split "\n"
                                -- find index of line where function or value is defined
                                |> List.Extra.findIndex
                                    (String.startsWith needle)
                                -- create constructor for Node expression passing start and end position
                                -- of its name in the source code
                                |> Maybe.map
                                    (\index ->
                                        Node -- Node is type defined in Elm.Syntax.Node
                                            -- Range {Location,Location}
                                            { start = { row = index + 1, column = 1 }
                                            , end = { row = index + 1, column = 1 + String.length name }
                                            }
                                    )
                                |> Maybe.withDefault fakeNode

                        _ ->
                            fakeNode
                -- passes expression wrapped in Node to expression evaluator
                -- together with environment env with declared functions and values, and trace flag
                ( result, callTrees, logLines ) =
                    -- expression evaluation
                    Eval.Expression.evalExpression
                        (maybeNode expression) -- expression to evaluate wrapped in Node
                        { trace = cfg.trace }
                        env -- environment with declared functions and values passed here
            in
            ( Result.mapError Types.EvalError result -- returned Result, if Err maps to EvalError
            , callTrees
            , logLines
            )

-- processes checked moduleSource code and builds environment with declared functions and values
buildInitialEnv : Maybe Env -> File -> Result Error Env
buildInitialEnv curEnv file =
    let
        moduleName : ModuleName
        moduleName =
            case Node.value file.moduleDefinition of
                NormalModule normal ->
                    Node.value normal.moduleName

                PortModule port_ ->
                    Node.value port_.moduleName

                EffectModule effect ->
                    Node.value effect.moduleName

        -- initial environment with basic or predefined declared functions and values
        -- as the ones set in my createTestEnvValues
        coreEnv : Env
        coreEnv = case curEnv of
            Just env -> env
            Nothing ->
                { currentModule = moduleName
                , callStack = []
                -- here func from my modules put in codegen/Elm are added to Core.functions
                --      e.g. Financial.elm, FuncDataArray.elm that duplicates the same module compiled in this app
                , functions = Core.functions
                , functionsInFormulas = Dict.empty
                -- values can be of any type variant included in Value (see Types.elm)
                -- here I add some test values to the environment
                -- NB: the values can be used by the interpreter, even if they are not declared in the source code!
                , values = XModel.createTestEnvValues
                , envXModel = Nothing
                , msgLine = ""
                }


        addDeclaration : Node Declaration -> Env -> Result Error Env
        addDeclaration (Node _ decl) env =
            case decl of
                FunctionDeclaration function ->
                    let
                        (Node _ implementation) =
                            function.declaration
                        funcName = getFuncName function.declaration
                        env1 = Environment.addFunction moduleName implementation env
                        env2 = Environment.addFunctionInFormulas moduleName funcName env1
                    in
                    -- Debug.log (Debug.toString moduleName ++ Debug.toString funcName)
                    Ok env2
                -- unsupported declarations
                PortDeclaration _ ->
                    Err <| Types.EvalError <| unsupported env "Port declaration"

                InfixDeclaration _ ->
                    Err <| Types.EvalError <| unsupported env "Infix declaration"

                Destructuring _ _ ->
                    Err <| Types.EvalError <| unsupported env "Top level destructuring"

                AliasDeclaration _ ->
                    Ok env -- aliases are not managed by the interpreter, env is not updated

                CustomTypeDeclaration _ ->
                    Ok env -- custom types are not managed by the interpreter, env is not updated
    in
    -- loops on declarations in source code and add them to the environment
    -- env is intialized with coreEnv and then updated with declarations
    -- if they are managed by the interpreter (functions are managed, ports are not managed, etc.)
    file.declarations
        |> Result.MyExtra.combineFoldl
            addDeclaration
            (Ok coreEnv)
emptyEnv : Env
emptyEnv =
    { currentModule = []
    , callStack = []
    , functions = Dict.empty
    , functionsInFormulas = Dict.empty
    , values = Dict.empty
    , envXModel = Nothing
    , msgLine = ""
    }

emptyEnvWithCoreFunctions : Env
emptyEnvWithCoreFunctions =
    { currentModule = []
    , callStack = []
    , functions = Core.functions
    , functionsInFormulas = Dict.empty
    , values = Dict.empty
    , envXModel = Nothing
    , msgLine = ""
    }


-- added by Luca
getFuncName : Node FunctionImplementation -> String
getFuncName (Node _ funcImpl) =
    case funcImpl.name of
        (Node _ retName) -> retName

