module Eval.Eval

open Parser.Syntax
open Value

exception RuntimeError of string

let rec private evalExpr (env: ValEnv) (expr: Expr) : Value =
    match expr with
    | ExprId id ->
        match ValEnv.tryFind id env with
        | Some v -> v
        | None ->
            let msg =
                $"val \'{id}\' is undefined.\nexpression: {expr}"

            raise (RuntimeError msg)
    | ExprInt i -> ValInt i
    | ExprString str -> ValString str
    | ExprTrue -> ValBool true
    | ExprFalse -> ValBool false
    | ExprFn (id, expr') -> ValCls(env, id, expr') // 外側（親ノード）のスコープの環境を参照する。ExprFixも同様。
    | ExprFix (fnId, domId, expr') -> ValRec(env, fnId, domId, expr')
    | ExprApp (expr1, expr2) ->
        let v1 = evalExpr env expr1
        let v2 = evalExpr env expr2

        match v1 with
        | ValCls (env1, domId, expr1') -> evalExpr (ValEnv.add domId v2 env1) expr1'
        | ValRec (env1, fnId, domId, expr1') ->
            let newEnv =
                env1 |> ValEnv.add fnId v1 |> ValEnv.add domId v2

            evalExpr newEnv expr1'
        | _ ->
            let msg =
                $"val {v1} is neither ValCls nor ValRec.\nexpression: {expr}"

            raise (RuntimeError msg)
    | ExprProj1 expr' ->
        let v = evalExpr env expr'

        match v with
        | ValPair (v1, v2) -> v1
        | _ ->
            let msg =
                $"val {v} is not ValPair.\nexpression: {expr}"

            raise (RuntimeError msg)
    | ExprProj2 expr' ->
        let v = evalExpr env expr'

        match v with
        | ValPair (v1, v2) -> v2
        | _ ->
            let msg =
                $"val {v} is not ValPair.\nexpression: {expr}"

            raise (RuntimeError msg)
    | ExprPair (expr1, expr2) ->
        let v1 = evalExpr env expr1
        let v2 = evalExpr env expr2
        ValPair(v1, v2)
    | ExprIf (cond, conseq, alt) ->
        let v1 = evalExpr env cond

        match v1 with
        | ValBool true -> evalExpr env conseq
        | ValBool false -> evalExpr env alt
        | _ ->
            let msg =
                $"val {v1} is not ValBool.\nexpression: {expr}"

            raise (RuntimeError msg)
    | ExprPrim (op, expr1, expr2) ->
        let v1 = evalExpr env expr1
        let v2 = evalExpr env expr2

        let (i1, i2) =
            match v1, v2 with
            | ValInt i1, ValInt i2 -> i1, i2
            | _ ->
                let msg =
                    $"val {v1} or {v2} is not ValInt.\nexpression: {expr}"

                raise (RuntimeError msg)

        match op with
        | Add -> ValInt(i1 + i2)
        | Sub -> ValInt(i1 - i2)
        | Mul -> ValInt(i1 * i2)
        | Div -> ValInt(i1 / i2)
        | Eq -> ValBool(i1 = i2)

let eval (env: ValEnv) (Ast dec) : ValEnv =
    let (id, expr) =
        match dec with
        | Val (id, expr) -> id, expr

    let v = evalExpr env expr
    printfn $"Evaluated to:\nval {id} = {v}"
    ValEnv.add id v env
