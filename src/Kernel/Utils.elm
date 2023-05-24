module Kernel.Utils exposing (append, compare, comparison)

import Value exposing (Env, EvalResult, Value(..), typeError)


append : Env -> Value -> Value -> EvalResult Value
append env l r =
    case ( l, r ) of
        ( String ls, String rs ) ->
            Ok <| String (ls ++ rs)

        ( List ll, List rl ) ->
            Ok <| List (ll ++ rl)

        _ ->
            typeError env <| "Cannot append " ++ Value.toString l ++ " and " ++ Value.toString r


compare : Env -> Value -> Value -> EvalResult Order
compare env l r =
    let
        inner : comparable -> comparable -> EvalResult Order
        inner lv rv =
            Ok <| Basics.compare lv rv
    in
    case ( l, r ) of
        -- TODO: Implement all cases
        ( Int lv, Int rv ) ->
            inner lv rv

        ( Float lv, Float rv ) ->
            inner lv rv

        ( Int lv, Float rv ) ->
            inner (toFloat lv) rv

        ( Float lv, Int rv ) ->
            inner lv (toFloat rv)

        ( String lv, String rv ) ->
            inner lv rv

        ( Char lv, Char rv ) ->
            inner lv rv

        ( Tuple la lb, Tuple ra rb ) ->
            compare env la ra
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            compare env lb rb
                    )

        ( Triple la lb lc, Triple ra rb rc ) ->
            compare env la ra
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            compare env lb rb
                                |> Result.andThen
                                    (\b ->
                                        if b /= EQ then
                                            Ok b

                                        else
                                            compare env lc rc
                                    )
                    )

        ( List [], List (_ :: _) ) ->
            Ok LT

        ( List (_ :: _), List [] ) ->
            Ok GT

        ( List [], List [] ) ->
            Ok EQ

        ( List (lh :: lt), List (rh :: rt) ) ->
            compare env lh rh
                |> Result.andThen
                    (\h ->
                        if h /= EQ then
                            Ok h

                        else
                            compare env (List lt) (List rt)
                    )

        _ ->
            case ( Value.toArray l, Value.toArray r ) of
                ( Just la, Just ra ) ->
                    compare env (List la) (List ra)

                _ ->
                    typeError env <|
                        "Comparison not yet implemented for "
                            ++ Value.toString l
                            ++ " and "
                            ++ Value.toString r


comparison : List Order -> ( Int, Env -> List Value -> EvalResult Value )
comparison orders =
    ( 2
    , \env args ->
        case args of
            [ l, r ] ->
                Result.map (\result -> Bool (List.member result orders)) <| compare env l r

            _ ->
                typeError env "Comparison needs exactly two arguments"
    )