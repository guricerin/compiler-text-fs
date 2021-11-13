open System
open System.IO

open FSharp.CommandLine

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

let repl (reader: StreamReader) (isInteractive: bool) =
    let rec go gamma env =
        let (newGamma, newEnv) =
            if isInteractive then printf "# "
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

    go TyEnv.empty ValEnv.empty |> ignore

let rcpl (reader: StreamReader) (isInteractive: bool) =
    let rec go gamma env =
        let (newGamma, newEnv) =
            if isInteractive then printf "# "
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

    go TyEnv.empty SecdEnv.empty |> ignore

type Mode =
    | Interpret
    | Compile

let modeOption =
    commandOption {
        names [ "m"; "mode" ]
        description "Compile or Interpret (default mode: Compile)."
        takes (regex @"c" |> asConst Compile)
        takes (regex @"i" |> asConst Interpret)
    }

let fileOption =
    commandOption {
        names [ "l"; "load-file" ]
        description "Path to a CoreML file. Without this option, you'll be into interactive mode."
        takes (format ("%s"))
        suggests (fun _ -> [ CommandSuggestion.Files None ])
    }

let top (mode: Mode) (path: option<string>) =
    match path with
    | Some path ->
        use reader =
            new StreamReader(path, Text.Encoding.UTF8)

        match mode with
        | Compile -> rcpl reader false
        | Interpret -> repl reader false
    | None ->
        use reader =
            new StreamReader(Console.OpenStandardInput(), Text.Encoding.UTF8)

        match mode with
        | Compile -> rcpl reader true
        | Interpret -> repl reader true

let mainCommand () =
    command {
        name "CoreML"
        description "  CoreML Processor."

        opt file in fileOption |> CommandOption.zeroOrExactlyOne

        opt mode in modeOption
                    |> CommandOption.zeroOrExactlyOne
                    |> CommandOption.whenMissingUse Compile

        try
            top mode file
            return 0
        with
        | ex ->
            eprintfn $"{ex}"
            return 1
    }

[<EntryPoint>]
let main argv =
    mainCommand () |> Command.runAsEntryPoint argv
