module TypeInf.TypeUtils

open Type

/// 型代入 : 型変数の有限集合から型への写像
type Subst = Subst of Map<string, Ty>

[<RequireQualifiedAccess>]
module Subst =
    let empty = Map.empty |> Subst

    /// 型代入の適用
    let rec apply (subst: Subst) (ty: Ty) : Ty =
        match ty with
        | TyInt
        | TyString
        | TyBool -> ty
        | TyVar tid ->
            match subst with
            | Subst subst ->
                match Map.tryFind tid subst with
                | Some ty' -> ty'
                | None -> ty
        | TyFun (ty1, ty2) -> TyFun(apply subst ty1, apply subst ty2)
        | TyPair (ty1, ty2) -> TyPair(apply subst ty1, apply subst ty2)
        | TyPoly (tids, ty) -> TyPoly(tids, apply subst ty)

    let singleton (tid, ty) = [ (tid, ty) ] |> Map.ofList |> Subst

    /// 2つの型代入の合成
    let compose (Subst subst1) (Subst subst2) : Subst =
        let subst3 =
            Map.fold
                (fun acc k v ->
                    let v' = apply (Subst subst1) v
                    Map.add k v' acc)
                Map.empty
                subst2

        let res =
            Map.fold
                (fun acc k v ->
                    match Map.tryFind k acc with
                    | Some v' -> acc
                    | None -> Map.add k v acc)
                subst3
                subst1

        res |> Subst

/// 型環境 : 変数の有限集合から型への写像
type TyEnv =
    | TyEnv of Map<string, Ty>
    override this.ToString() =
        match this with
        | TyEnv mp ->
            mp
            |> Map.toList
            |> List.map (fun (tid, ty) -> $"{tid}:{ty}")
            |> String.concat " , "
            |> fun x -> sprintf "{%s}" x

[<RequireQualifiedAccess>]
module TyEnv =
    let empty = Map.empty |> TyEnv

    let tryFind (TyEnv env) (varName: string) : option<Ty> = Map.tryFind varName env

    /// 型環境に型代入を適用
    let subst (Subst senv) (TyEnv tyenv) : TyEnv =
        Map.fold
            (fun acc k v ->
                let v' = Subst.apply (Subst senv) v
                Map.add k v' acc)
            Map.empty
            tyenv
        |> TyEnv

    let singleton (tid, ty) = [ (tid, ty) ] |> Map.ofList |> TyEnv

    /// 2つの型環境の積集合
    /// 同じ変数に型付けされた型の組の集合
    let matches (TyEnv tyenv1) (TyEnv tyenv2) =
        Map.fold
            (fun acc var ty ->
                match Map.tryFind var tyenv2 with
                | Some ty' -> Set.add (ty, ty') acc
                | None -> acc)
            Set.empty
            tyenv1
        |> List.ofSeq

    /// 2つの型環境の和集合
    let union (TyEnv tyenv1) (TyEnv tyenv2) : TyEnv =
        Map.fold
            (fun acc k v ->
                match Map.tryFind k acc with
                // tyenv1の要素を優先
                | Some v' -> acc
                | None -> Map.add k v acc)
            tyenv1
            tyenv2
        |> TyEnv

    let remove (TyEnv tyenv) (varName: string) : TyEnv = Map.remove varName tyenv |> TyEnv

    let add (varName: string) (ty: Ty) (TyEnv tyenv) : TyEnv = Map.add varName ty tyenv |> TyEnv

/// 名前の衝突を防ぐために、既存の型変数を新たな型変数に置き換える
/// 衝突したら型の多相性が失われるため
let freshInst (ty: Ty) : Ty =
    match ty with
    | TyPoly (tids, ty') ->
        let subst =
            List.foldBack
                (fun tid acc ->
                    let newTy = _tyVarHelper.NewTy()
                    Map.add tid newTy acc)
                tids
                Map.empty
            |> Subst

        Subst.apply subst ty'
    | _ -> ty
