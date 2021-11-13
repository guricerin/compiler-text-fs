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
    | IMkCls of string * Code
    | IMkRec of string * string * Code
    | IIf of Code * Code
    /// 関数からのリターン命令
    | IRet
    override this.ToString() = sprintf "%A" this

and Code =
    | Code of Inst list
    override this.ToString() = sprintf "%A" this

[<RequireQualifiedAccess>]
module Code =
    let empty = [] |> Code

    let push (inst: Inst) (Code k) : Code = inst :: k |> Code

    let append (Code c) (Code k) : Code = List.append c k |> Code
