module Environment exposing (addFunction, addFunctionInFormulas, addValue, call, empty, with)

import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import FastDict as Dict
import Types exposing (Env, EnvValues, Value, RecalcState(..))


addValue : String -> Value -> Env -> Env
addValue name value env =
    { env
        | values = Dict.insert name value env.values
    }


addFunction : ModuleName -> FunctionImplementation -> Env -> Env
addFunction moduleName function env =
    { env
        | functions =
            Dict.insert
                moduleName
                (Dict.insert (Node.value function.name)
                    function
                    (Maybe.withDefault Dict.empty
                        (Dict.get moduleName env.functions)
                    )
                )
                env.functions
    }


addFunctionInFormulas : ModuleName -> String -> Env -> Env
addFunctionInFormulas moduleName funcName env =
    let
        curCalcOrder =
            Maybe.withDefault []
                (Dict.get moduleName env.functionsInFormulas
                    |> Maybe.map .functionCalcOrder)
        updatedCalcOrder =
            curCalcOrder ++ [funcName]
        updatedFunctionInfo =
            { calcOrder = List.length updatedCalcOrder
            , recalcState = Types.RecalcNeeded
            }
        updatedModuleInfo =
            { functionDict =
                Dict.insert
                    funcName
                    updatedFunctionInfo
                    (Maybe.withDefault Dict.empty
                        (Dict.get moduleName env.functionsInFormulas
                            |> Maybe.map .functionDict
                        )
                    )
            , functionCalcOrder = updatedCalcOrder
            }
    in
    { env
        | functionsInFormulas =
            Dict.insert
                moduleName
                updatedModuleInfo
                env.functionsInFormulas
    }


with : EnvValues -> Env -> Env
with newValues old =
    { old | values = Dict.union newValues old.values }


empty : ModuleName -> Env
empty moduleName =
    { currentModule = moduleName
    , callStack = []
    , functions = Dict.empty
    , functionsInFormulas = Dict.empty
    , values = Dict.empty
    , envXModel = Nothing
    , msgLine = ""
    }


call : ModuleName -> String -> Env -> Env
call moduleName name env =
    { env
        | currentModule = moduleName
        , callStack =
            { moduleName = moduleName, name = name }
                :: env.callStack
    }
