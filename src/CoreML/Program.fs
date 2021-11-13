open System
open System.IO

open Parser
open TypeInf
open TypeUtils
open Eval
open Eval.Value
open SECD
open SECD.Value

let doInterpret text gamma env =
    let ast = ParserFacade.parse text
    printfn $"Parse Result:\n{ast}"
    let (newGamma, id, ty) = TypeInf.polyTypeInf gamma ast
    printfn $"Inferred Typing:\nval {id} : {ty}"
    let (newEnv, id, value) = Eval.eval env ast
    printfn $"Evaluated to:\nval {id} = {value}"
    printfn ""
    newGamma, newEnv

let doCompile text gamma env =
    let ast = ParserFacade.parse text
    printfn $"Parse Result:\n{ast}"
    let (newGamma, id, ty) = TypeInf.polyTypeInf gamma ast
    printfn $"Inferred Typing:\nval {id} : {ty}"
    let (id, code) as namedCode = Comp.compile ast
    printfn $"Compiled to:\n{code}"
    let (newEnv, id, value) = Exec.run env namedCode
    printfn $"Execution result:\nval {id} = {value}"
    printfn ""
    newGamma, newEnv

let repl (reader: StreamReader) (isInterctive: bool) =
    let rec go gamma env =
        let (newGamma, newEnv) =
            if isInterctive then printf "# "
            let line = reader.ReadLine()

            if String.IsNullOrWhiteSpace(line) then
                gamma, env
            else
                try
                    doInterpret line gamma env
                with
                | ex ->
                    eprintfn "%A\n" ex
                    gamma, env

        go newGamma newEnv

    go TyEnv.empty ValEnv.empty

let rcpl (reader: StreamReader) (isInterctive: bool) =
    let rec go gamma env =
        let (newGamma, newEnv) =
            if isInterctive then printf "# "
            let line = reader.ReadLine()

            if String.IsNullOrWhiteSpace(line) then
                gamma, env
            else
                try
                    doCompile line gamma env
                with
                | ex ->
                    eprintfn "%A\n" ex
                    gamma, env

        go newGamma newEnv

    go TyEnv.empty SecdEnv.empty

let printUsage () =
    let msg =
        "USAGE:
  dotnet run <i|c> [path]

FLAGS:
  i       interpret
  c       compile to SECD VM lang

OPTIONS:
  path    CoreML file path"

    printfn "%s" msg

let top (argv: string array) =
    match argv.Length with
    | 1 ->
        use reader =
            new StreamReader(Console.OpenStandardInput(), Text.Encoding.UTF8)

        match argv.[0] with
        | "i" -> repl reader true
        | "c" -> rcpl reader true
        | _ -> printUsage ()
    | 2 ->
        use reader =
            new StreamReader(argv.[1], Text.Encoding.UTF8)

        match argv.[0] with
        | "i" -> repl reader false
        | "c" -> rcpl reader false
        | _ -> printUsage ()
    | _ -> printUsage ()

[<EntryPoint>]
let main argv =
    try
        top argv
        0
    with
    | ex ->
        eprintfn "%A" ex
        1
