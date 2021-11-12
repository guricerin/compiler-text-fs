module Eval.Value

open Parser.Syntax

type Value =
    | ValInt of int
    | ValBool of bool
    | ValString of string
    | ValPair of Value * Value
    /// 関数クロージャ
    | ValCls of ValEnv * string * Expr
    /// 再帰関数
    | ValRec of ValEnv * string * string * Expr
    override this.ToString() =
        match this with
        | ValInt i -> $"{i}"
        | ValBool b -> $"{b}"
        | ValString s -> sprintf "\"%s\"" s
        | ValPair (v1, v2) -> $"({v1},{v2})"
        | ValCls (env, x, expr) -> "fn"
        | ValRec (env, f, x, expr) -> "fix"

/// 変数：値
and ValEnv = ValEnv of Map<string, Value>

[<RequireQualifiedAccess>]
module ValEnv =
    let empty = Map.empty |> ValEnv

    let add (varId: string) (v: Value) (ValEnv env) = Map.add varId v env |> ValEnv

    let tryFind (varId: string) (ValEnv env) = Map.tryFind varId env
