module SECD.Exec

open Value
open Instruction

exception RuntimeError of string

let rec private exec (stack: SecdValue list) (env: SecdEnv) (Code code) (dump: (Code * SecdEnv) list) : SecdValue =
    match stack, env, code, dump with
    | v :: _, _, [], [] -> v
    | _, _, IAcc varId :: code', _ ->
        match SecdEnv.tryFind varId env with
        | Some (v) -> exec (v :: stack) env (Code code') dump
        | None ->
            let msg = $"var '{varId} is undefined."
            raise (RuntimeError msg)
    | _, _, IPushI i :: code', _ -> exec ((SvInt i) :: stack) env (Code code') dump
    | _, _, IPushS str :: code', _ -> exec ((SvString str) :: stack) env (Code code') dump
    | _, _, IPushB b :: code', _ -> exec ((SvBool b) :: stack) env (Code code') dump
    | _, _, IMkCls (domId, code0) :: code', _ -> exec ((SvCls(env, domId, code0)) :: stack) env (Code code') dump
    | _, _, IMkRec (fnId, domId, code0) :: code', _ ->
        exec ((SvRec(env, fnId, domId, code0)) :: stack) env (Code code') dump
    | v :: SvCls (env0, domId, code0) :: stack', _, IApp :: code', _ ->
        let env' = SecdEnv.add domId v env0
        // 戻り先情報をdumpに格納して関数本体を実行
        exec stack' env' code0 (((Code code'), env) :: dump)
    | v1 :: (SvRec (env0, fnId, domId, code0) as v2) :: stack', _, IApp :: code', _ ->
        let env' =
            env0
            |> SecdEnv.add fnId v2 // 自己参照
            |> SecdEnv.add domId v1
        // 戻り先情報をdumpに格納して関数本体を実行
        exec stack' env' code0 (((Code code'), env) :: dump)
    | v1 :: v2 :: stack', _, IPair :: code', _ -> exec (SvPair(v2, v1) :: stack') env (Code code') dump // 順番に注意
    | SvPair (v1, _) :: stack', _, IProj1 :: code', _ -> exec (v1 :: stack') env (Code code') dump
    | SvPair (_, v2) :: stack', _, IProj2 :: code', _ -> exec (v2 :: stack') env (Code code') dump
    | SvInt i1 :: SvInt i2 :: stack', _, IPrim (op) :: code', _ ->
        let v =
            match op with
            // 順番に注意
            | SecdEq -> SvBool(i2 = i1)
            | SecdAdd -> SvInt(i2 + i1)
            | SecdSub -> SvInt(i2 - i1)
            | SecdMul -> SvInt(i2 * i1)
            | SecdDiv -> SvInt(i2 / i1)

        exec (v :: stack') env (Code code') dump
    | v :: stack', _, IRet :: code', (code0, env0) :: dump' -> exec (v :: stack') env0 code0 dump'
    | SvBool true :: stack', _, IIf (conseq, _) :: code', _ -> exec stack' env (Code.append conseq (Code code')) dump
    | SvBool false :: stack', _, IIf (_, alt) :: code', _ -> exec stack' env (Code.append alt (Code code')) dump
    | _ ->
        let msg = $"runtime error.\ncode: {code}"
        raise (RuntimeError msg)

/// 中間言語表現を実行
let run (env: SecdEnv) ((valId, code): string * Code) : (SecdEnv * string * SecdValue) =
    let v = exec [] env code []
    let newEnv = SecdEnv.add valId v env
    newEnv, valId, v
