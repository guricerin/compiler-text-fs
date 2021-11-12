open System
open System.IO

open Parser
open TypeInf
open TypeUtils
open Eval
open Value

let parseTyinfPrint text gamma env =
    let ast = text |> ParserFacade.parse
    printfn $"Parse Result:\n{ast}"
    let newGamma = TypeInf.polyTypeInf gamma ast
    let newEnv = Eval.eval env ast
    printfn ""
    newGamma, newEnv

let repFile (filename: string) =
    use reader =
        new StreamReader(filename, Text.Encoding.UTF8)

    let rec go gamma env =
        let (newGamma, newEnv) =
            match reader.ReadLine() with
            | null -> gamma, env
            | line ->
                try
                    parseTyinfPrint line gamma env
                with
                | ex ->
                    eprintfn "%A\n" ex
                    gamma, env

        go newGamma newEnv

    go TyEnv.empty ValEnv.empty

let repl () =
    let rec go gamma env =
        printf "CoreML> "

        let (newGamma, newEnv) =
            match Console.ReadLine() with
            | null -> gamma, env
            | line ->
                try
                    parseTyinfPrint line gamma env
                with
                | ex ->
                    eprintfn "%A\n" ex
                    gamma, env

        go newGamma newEnv

    go TyEnv.empty ValEnv.empty

[<EntryPoint>]
let main argv =
    if argv.Length = 1 then
        repFile argv.[0]
    else
        repl ()

    0 // return an integer exit code
