module TypeInf.UnifyTy

open Type
open TypeUtils

/// 指定の型の中に現れる型変数の集合
let ftv (ty: Ty) : Set<string> =
    let rec scan ty se =
        match ty with
        | TyVar tid -> Set.add tid se
        | Fun (domTy, ranTy) -> se |> scan domTy |> scan ranTy
        | Pair (fstTy, sndTy) -> se |> scan fstTy |> scan sndTy
        | _ -> se

    scan ty Set.empty

/// 指定の型変数が FTV(ty) に含まれるかを判定
let private occurs (tyvar: Ty) (ty: Ty) : bool =
    match tyvar with
    | TyVar tid -> Set.contains tid (ftv ty)
    | _ -> false

/// 型の単一化に失敗したことを報告する
/// 単一化 : ある式に現れるすべての型変数を、同じ型変数に置き換えること
exception UnifyTyErrorException of string

let private makeUnifyTyError ty1 ty2 =
    let msg =
        $"failed to unifyTy:\nty1: {ty1}\nty2: {ty2}"

    UnifyTyErrorException msg

/// 型変数への代入（型代入の適用）と等式の変形を繰り返す
let rec private rewrite (e: (Ty * Ty) list) (s: Subst) : Subst =
    match e with
    | [] -> s
    | (ty1, ty2) :: tail ->
        if ty1 = ty2 then
            rewrite tail s
        else
            match ty1, ty2 with
            | TyVar tid, _ ->
                if occurs ty1 ty2 then
                    raise (makeUnifyTyError ty1 ty2)
                else
                    let s1 = Subst.singleton (tid, ty2)
                    let s2 = Subst.compose s1 s

                    let e2 =
                        List.map (fun (ty1, ty2) -> (Subst.substTy s1 ty1, Subst.substTy s1 ty2)) tail

                    rewrite e2 s2
            | _, TyVar tid -> rewrite ((ty2, ty1) :: tail) s
            | Fun (ty11, ty12), Fun (ty21, ty22) -> rewrite ((ty11, ty21) :: (ty12, ty22) :: tail) s
            | Pair (ty11, ty12), Pair (ty21, ty22) -> rewrite ((ty11, ty21) :: (ty12, ty22) :: tail) s
            | _ -> raise (makeUnifyTyError ty1 ty2)

/// 型の単一化（unifycation）
/// すべての (t1,t2) (e E について、S(t1) = S(t2) となる型変数への代入 S を E の単一化という
let unify (e: (Ty * Ty) list) : Subst = rewrite e Subst.empty
