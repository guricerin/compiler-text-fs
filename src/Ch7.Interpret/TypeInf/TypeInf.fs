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
    | ExprInt i -> (TyEnv.empty, TyInt)
    | ExprString str -> (TyEnv.empty, TyString)
    | ExprTrue
    | ExprFalse -> (TyEnv.empty, TyBool)
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

        (tyEnv3, Subst.apply subst (TyPair(ty1, ty2)))
    | ExprApp (expr1, expr2) ->
        let (tyEnv1, ty1) = pts expr1
        let (tyEnv2, ty2) = pts expr2
        let tyEquations = TyEnv.matches tyEnv1 tyEnv2
        let newTy = _tyVarHelper.NewTy()

        let subst =
            unify ((TyFun(ty2, newTy), ty1) :: tyEquations)

        let tyEnv3 =
            TyEnv.union (TyEnv.subst subst tyEnv1) (TyEnv.subst subst tyEnv2)

        (tyEnv3, Subst.apply subst newTy)
    | ExprFn (fnName, expr) ->
        let (tyEnv, ty) = pts expr

        match TyEnv.tryFind tyEnv fnName with
        | Some domty -> (TyEnv.remove tyEnv fnName, TyFun(domty, ty))
        | None -> (tyEnv, TyFun(_tyVarHelper.NewTy(), ty))
    | _ -> raise (makeTypeError ())

/// 単相型推論（type inference）
let singleTypeinf (Ast dec) =
    let expr =
        match dec with
        | Val (id, expr) -> expr

    let (tyEnv, ty) = pts expr
    printfn $"Inferred Typing:\n{tyEnv}|- {expr} : {ty}"

/// 多相型推論アルゴリズム W
/// 型環境 gamma の下で、式 expr が持つ型と、gammaに含まれる型変数の制約を表現する型代入 S の組を計算
/// 呼び出し前の式 expr の型 t と型環境 gamma は、呼び出し後においては、S(t) および S(gamma) となる
let rec private w (gamma: TyEnv) (expr: Expr) : Subst * Ty =
    match expr with
    | ExprInt i -> Subst.empty, TyInt
    | ExprString str -> Subst.empty, TyString
    | ExprTrue
    | ExprFalse -> Subst.empty, TyBool
    | ExprId varId ->
        match TyEnv.tryFind gamma varId with
        | Some ty -> Subst.empty, freshInst ty
        | None -> raise (makeTypeError ())
    | ExprFn (fnId, expr') ->
        let ty1 = _tyVarHelper.NewTy()
        let newGamma = TyEnv.add fnId ty1 gamma
        let (s, ty2) = w newGamma expr'
        s, TyFun(Subst.apply s ty1, ty2)
    | ExprApp (expr1, expr2) ->
        let (s1, ty1) = w gamma expr1
        let (s2, ty2) = w (TyEnv.subst s1 gamma) expr2
        let ty3 = _tyVarHelper.NewTy()

        let s3 =
            unify [ (TyFun(ty2, ty3), Subst.apply s2 ty1) ]

        let s4 = Subst.compose s3 (Subst.compose s2 s1)
        s4, Subst.apply s4 ty3
    | ExprPair (expr1, expr2) ->
        let (s1, ty1) = w gamma expr1
        let (s2, ty2) = w (TyEnv.subst s1 gamma) expr2
        Subst.compose s2 s1, TyPair(Subst.apply s2 ty1, ty2)
    | ExprProj1 expr' ->
        let (s1, ty) = w gamma expr'
        let ty1 = _tyVarHelper.NewTy()
        let ty2 = _tyVarHelper.NewTy()
        let s2 = unify [ (ty, TyPair(ty1, ty2)) ]
        Subst.compose s2 s1, Subst.apply s2 ty1
    | ExprProj2 expr' ->
        let (s1, ty) = w gamma expr'
        let ty1 = _tyVarHelper.NewTy()
        let ty2 = _tyVarHelper.NewTy()
        let s2 = unify [ (ty, TyPair(ty1, ty2)) ]
        Subst.compose s2 s1, Subst.apply s2 ty2
    | ExprIf (cond, conseq, alt) ->
        let (s1, ty1) = w gamma cond
        let s2 = unify [ (ty1, TyBool) ]

        let (s3, ty2) =
            w (TyEnv.subst (Subst.compose s2 s1) gamma) conseq

        let (s4, ty3) =
            w (TyEnv.subst (Subst.compose s2 s1 |> Subst.compose s3) gamma) alt

        let s5 = unify [ (ty2, ty3) ]

        let s =
            s1
            |> Subst.compose s2
            |> Subst.compose s3
            |> Subst.compose s4
            |> Subst.compose s5

        let newGamma = TyEnv.subst s gamma
        s, Subst.apply s5 ty2
    | ExprFix (funId, argId, expr') ->
        let argTy = _tyVarHelper.NewTy()
        let bodyTy = _tyVarHelper.NewTy()
        let funTy = TyFun(argTy, bodyTy)

        let newGamma =
            gamma
            |> TyEnv.add funId funTy
            |> TyEnv.add argId argTy

        let (s1, ty) = w newGamma expr'
        let s2 = unify [ (ty, bodyTy) ]
        let s = Subst.compose s2 s1
        s, Subst.apply s funTy
    | ExprPrim (op, expr1, expr2) ->
        let (s1, ty1) = w gamma expr1
        let (s2, ty2) = w (TyEnv.subst s1 gamma) expr2

        let s3 =
            unify [ (Subst.apply s2 ty1, TyInt)
                    (ty2, TyInt) ]

        let ty3 =
            match op with
            | Eq -> TyBool
            | _ -> TyInt

        s1 |> Subst.compose s2 |> Subst.compose s3, ty3

let polyTypeInf (gamma: TyEnv) (Ast dec) =
    let (id, expr) =
        match dec with
        | Val (id, expr) -> id, expr

    let (subst, ty) = w gamma expr
    let tids = ftv ty |> List.ofSeq

    let newTy =
        match tids with
        | [] -> ty
        | _ -> TyPoly(tids, ty)

    printfn $"Inferred Typing:\nval {id} : {newTy}"
    TyEnv.add id newTy gamma
