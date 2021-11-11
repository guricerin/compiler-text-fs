module TypeInf.TypeInf

open Parser.Syntax
open Type
open TypeUtils
open UnifyTy

exception TypeError of string

let makeTypeError () = TypeError "type error"

/// 型推論アルゴリズム PTS
/// 任意の式 e に対して、もし e が型判定を持てば、その主要な型判定を計算する
/// 持なければ例外エラー
let rec private pts (absyn: Expr) : (TyEnv * Ty) =
    match absyn with
    | Expr.Int i -> (TyEnv.empty, Ty.Int)
    | Expr.EString str -> (TyEnv.empty, Ty.String)
    | Expr.True
    | Expr.False -> (TyEnv.empty, Ty.Bool)
    | ExprId tid ->
        let newTy = _tyVarHelper.NewTy()
        (TyEnv.singleton (tid, newTy), newTy)
    | ExprPair (expr1, expr2) ->
        let (tyEnv1, ty1) = pts expr1
        let (tyEnv2, ty2) = pts expr1
        let tyEquations = TyEnv.matches tyEnv1 tyEnv2
        let subst = unify tyEquations

        let tyEnv3 =
            TyEnv.union (TyEnv.subst subst tyEnv1) (TyEnv.subst subst tyEnv2)

        (tyEnv3, Subst.substTy subst (Ty.Pair(ty1, ty2)))
    | ExprApp (expr1, expr2) ->
        let (tyEnv1, ty1) = pts expr1
        let (tyEnv2, ty2) = pts expr2
        let tyEquations = TyEnv.matches tyEnv1 tyEnv2
        let newTy = _tyVarHelper.NewTy()

        let subst =
            unify ((Fun(ty2, newTy), ty1) :: tyEquations)

        let tyEnv3 =
            TyEnv.union (TyEnv.subst subst tyEnv1) (TyEnv.subst subst tyEnv2)

        (tyEnv3, Subst.substTy subst newTy)
    | ExprFn (fnName, expr) ->
        let (tyEnv, ty) = pts expr

        match TyEnv.tryFind tyEnv fnName with
        | Some domty -> (TyEnv.remove tyEnv fnName, Fun(domty, ty))
        | None -> (tyEnv, Fun(_tyVarHelper.NewTy(), ty))
    | _ -> raise (makeTypeError ())

/// 型推論（type inference）
let typeinf (Ast dec) =
    let expr =
        match dec with
        | Val (id, expr) -> expr

    let (tyEnv, ty) = pts expr
    printfn $"Inferred Typing:\n{tyEnv}|- {expr} : {ty}"
