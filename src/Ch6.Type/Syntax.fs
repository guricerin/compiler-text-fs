module Parser.Syntax

/// 組み込みの二項演算子
type PrimOp =
    | Eq
    | Add
    | Sub
    | Mul
    | Div
    override this.ToString() =
        match this with
        | Eq -> "eq"
        | Add -> "add"
        | Sub -> "sub"
        | Mul -> "mul"
        | Div -> "div"

type Expr =
    | ExprId of string
    | Int of int
    | EString of string
    | True
    | False
    | ExprFn of string * Expr
    | ExprApp of Expr * Expr
    | ExprPair of Expr * Expr
    | ExprProj1 of Expr
    | ExprProj2 of Expr
    | ExprPrim of PrimOp * Expr * Expr
    | ExprIf of Expr * Expr * Expr
    | ExprFix of string * string * Expr
    override this.ToString() =
        match this with
        | ExprId id -> id
        | Int i -> $"{i}"
        | EString s -> $"\"{s}\""
        | True -> "true"
        | False -> "false"
        | ExprFn (name, exp) -> $"(fn {name} => {exp})"
        | ExprApp (exp1, exp2) -> $"({exp1} {exp2})"
        | ExprIf (cond, conseq, alt) -> $"if {cond} then {conseq} else {alt}"
        | ExprProj1 exp -> $"#1 {exp}"
        | ExprProj2 exp -> $"#2 {exp}"
        | ExprFix (f, x, exp) -> $"(fix {f} ({x}) => {exp})"
        | ExprPrim (op, exp1, exp2) -> $"prim({op},{exp1},{exp2})"
        | ExprPair (exp1, exp2) -> $"({exp1},{exp2})"

/// 変数定義
type Dec =
    | Val of string * Expr
    override this.ToString() =
        match this with
        | Val (x, exp) -> $"val {x} = {exp}"

type Ast =
    | Ast of Dec
    override this.ToString() =
        match this with
        | Ast dec -> $"{dec}"
