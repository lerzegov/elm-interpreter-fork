module Kernel.List exposing (sortBy, sortWith)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Eval.Types as Types exposing (Eval)
import Kernel.Utils
import Rope
import Value exposing (Value)


sortBy : (Value -> Eval Value) -> List Value -> Eval (List Value)
sortBy toComparable list cfg env =
    Types.combineMap
        (\value icfg ienv ->
            Types.map
                (Tuple.pair value)
                (toComparable value icfg ienv)
        )
        list
        cfg
        env
        |> Types.map
            (List.sortWith
                (\( _, lc ) ( _, rc ) ->
                    case Kernel.Utils.innerCompare lc rc env of
                        Err e ->
                            handleErr e

                        Ok res ->
                            res
                )
                >> List.map Tuple.first
            )


sortWith : (Value -> Eval (Value -> Eval Order)) -> List Value -> Eval (List Value)
sortWith compare list cfg env =
    ( list
        |> List.sortWith
            (\lv rv ->
                case compare lv cfg env of
                    ( Err e, _, _ ) ->
                        handleErr e

                    ( Ok k, _, _ ) ->
                        case k rv cfg env of
                            ( Err e, _, _ ) ->
                                handleErr e

                            ( Ok res, _, _ ) ->
                                res
            )
        |> Ok
    , let
        _ =
            Debug.todo
      in
      []
    , let
        _ =
            Debug.todo
      in
      Rope.empty
    )


handleErr : { currentModule : ModuleName, callStack : List QualifiedNameRef, error : Value.EvalErrorKind } -> Order
handleErr err =
    let
        _ =
            Debug.log "handleErr" err
    in
    -- TODO: find out how to deal with errors
    Debug.todo "handleErr"
