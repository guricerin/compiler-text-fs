module SECD.Value

open Instruction

/// Stack に格納される
type SecdValue =
    | SvInt of int
    | SvString of string
    | SvBool of bool
    | SvPair of SecdValue * SecdValue
    | SvCls of SecdEnv * string * Code
    | SvRec of SecdEnv * string * string * Code
    override this.ToString() =
        match this with
        | SvInt i -> $"{i}"
        | SvString s -> $"\"{s}\""
        | SvBool b -> $"{b}"
        | SvPair (v1, v2) -> $"({v1},{v2})"
        | SvCls (_, _, _) -> "fn"
        | SvRec (_, _, _, _) -> "fix"

/// Environment そのもの
/// 現在の値の束縛を格納する
and SecdEnv = SecdEnv of Map<string, SecdValue>

[<RequireQualifiedAccess>]
module SecdEnv =
    let empty = Map.empty |> SecdEnv

    let tryFind (varId: string) (SecdEnv env) = Map.tryFind varId env

    let add (varId: string) (v: SecdValue) (SecdEnv env) = Map.add varId v env |> SecdEnv
