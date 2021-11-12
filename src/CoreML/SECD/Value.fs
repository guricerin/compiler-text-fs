module SECD.Value

open Instruction

/// Stack に格納される
type SecdValue =
    | VInt of int
    | VString of string
    | VBool of bool
    | VPair of SecdValue * SecdValue
    | VCls of SecdEnv * string * Code
    | VRec of SecdEnv * string * string * Code
    override this.ToString() =
        match this with
        | VInt i -> $"{i}"
        | VString s -> $"\"{s}\""
        | VBool b -> $"{b}"
        | VPair (v1, v2) -> $"({v1},{v2})"
        | VCls (_, _, _) -> "fn"
        | VRec (_, _, _, _) -> "fix"

/// Environment そのもの
/// 現在の値の束縛を格納する
and SecdEnv = SecdEnv of Map<string, SecdValue>

[<RequireQualifiedAccess>]
module SecdEnv =
    let empty = Map.empty |> SecdEnv
