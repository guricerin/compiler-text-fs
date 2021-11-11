module TypeInf.Type

// [<RequireQualifiedAccess>]
type Ty =
    /// 型変数
    | TyVar of string
    | Int
    | String
    | Bool
    | Fun of Ty * Ty
    | Pair of Ty * Ty
    | Poly of string list * Ty
    override this.ToString() =
        match this with
        | TyVar tid -> $"'{tid}"
        | Int -> "int"
        | String -> "string"
        | Bool -> "bool"
        | Fun (ty1, ty2) -> $"({ty1} -> {ty2})"
        | Pair (ty1, ty2) -> $"({ty1} * {ty2})"
        | Poly (tyIds, ty) ->
            let ids = String.concat "," tyIds
            $"[{ids}.{ty}]"

type TyVarHelper() =
    //let mutable _nextTyId = -1 // 最初のidを'a'にしたいとき
    let mutable _nextTyId = 0

    let newTyId () =
        _nextTyId <- _nextTyId + 1
        _nextTyId

    let rec numeral n =
        if n < 26 then
            [ int 'a' + n ]
        else
            let (msb, rest) = (n % 26, (n / 26) - 1)
            int 'a' + msb :: numeral rest

    let tyIdName tid =
        tid
        |> numeral
        |> List.rev
        |> List.map (char >> string)
        |> String.concat ""

    let newTyIdName () = newTyId () |> tyIdName

    member _.InitSeed() = _nextTyId <- 0
    member _.NewTy() = TyVar(newTyIdName ())

let _tyVarHelper = TyVarHelper()
