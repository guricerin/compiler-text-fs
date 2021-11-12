module SECD.Instruction

type SecdPrim =
    | SecdEq
    | SecdAdd
    | SecdSub
    | SecdMul
    | SecdDiv

/// SECD機械の命令
type Inst =
    | IPushI of int
    | IPushS of string
    | IPushB of bool
    /// 変数アクセス
    | IAcc of string
    | IApp
    | IPair
    | IProj1
    | IProj2
    | IPrim of SecdPrim
    | IMkCls of string * Inst list
    | IMkRec of string * string * Inst list
    | IIf of Inst list * Inst list
    /// 関数からのリターン命令
    | IRet
    override this.ToString() = sprintf "%A" this
// match this with
// | IPushI i -> $"PushI({i})"
// | IPushS s -> $"PushS({s})"
// | IPushB b -> $"PushB({b})"
// | IAcc varId -> $"Acc({varId})"
// | IApp -> "App"
// | IPair -> "Pair"
// | IProj1 -> "Proj1"
// | IProj2 -> "Proj2"
// | IPrim op ->
//     let op =
//         match op with
//         | SecdEq -> "EQ"
//         | SecdAdd -> "ADD"
//         | SecdSub -> "SUB"
//         | SecdMul -> "MUL"
//         | SecdDiv -> "DIV"

//     $"Prim({op})"
// | IMkCls(domId, code)->

type Code =
    | Code of Inst list
    override this.ToString() = sprintf "%A" this
