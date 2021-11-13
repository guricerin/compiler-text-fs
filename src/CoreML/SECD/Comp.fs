module SECD.Comp

open Parser.Syntax
open Instruction

let rec private comp (expr: Expr) (k: Code) : Code =
    match expr with
    | ExprInt i -> Code.push (IPushI i) k
    | ExprString str -> Code.push (IPushS str) k
    | ExprTrue -> Code.push (IPushB true) k
    | ExprFalse -> Code.push (IPushB false) k
    | ExprId varId -> Code.push (IAcc varId) k
    | ExprFn (domId, expr') ->
        let code =
            Code.empty |> Code.push IRet |> comp expr'

        Code.push (IMkCls(domId, code)) k
    | ExprFix (fnId, domId, expr') ->
        let code =
            Code.empty |> Code.push IRet |> comp expr'

        Code.push (IMkRec(fnId, domId, code)) k
    | ExprApp (expr1, expr2) -> k |> Code.push IApp |> comp expr2 |> comp expr1
    | ExprPair (expr1, expr2) -> k |> Code.push IPair |> comp expr2 |> comp expr1
    | ExprProj1 expr' -> k |> Code.push IProj1 |> comp expr'
    | ExprProj2 expr' -> k |> Code.push IProj2 |> comp expr'
    | ExprPrim (op, expr1, expr2) ->
        let op' =
            match op with
            | Eq -> SecdEq
            | Add -> SecdAdd
            | Sub -> SecdSub
            | Mul -> SecdMul
            | Div -> SecdDiv

        k
        |> Code.push (IPrim op')
        |> comp expr2
        |> comp expr1
    | ExprIf (cond, conseq, alt) ->
        let conseq' = comp conseq Code.empty
        let alt' = comp alt Code.empty
        let code = Code.push (IIf(conseq', alt')) k
        comp cond code

/// CoreMLコードをSCCD機械の中間言語表現にコンパイル
/// トップレベル宣言の変数名と、Codeスタックを返す
let compile (Ast dec) : string * Code =
    let (id, expr) =
        match dec with
        | Val (id, expr) -> id, expr

    let code = comp expr Code.empty
    printfn $"Compiled to:\n{code}"
    id, code
