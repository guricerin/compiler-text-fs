open System
open System.IO
open Parser
open TypeInf
open TypeUtils

let parseTyinfPrint text gamma =
    let ast = text |> ParserFacade.parse
    printfn $"Parse Result:\n{ast}"
    let newGamma = TypeInf.polyTypeInf gamma ast
    printfn ""
    newGamma

let repFile (filename: string) =
    use reader =
        new StreamReader(filename, Text.Encoding.UTF8)

    let rec go gamma =
        let newGamma =
            match reader.ReadLine() with
            | null -> gamma
            | line ->
                try
                    parseTyinfPrint line gamma
                with
                | ex ->
                    eprintfn "%A\n" ex
                    gamma

        go newGamma

    go TyEnv.empty

let repl () =
    let rec go gamma =
        printf "CoreML> "

        let newGamma =
            match Console.ReadLine() with
            | null -> gamma
            | line ->
                try
                    parseTyinfPrint line gamma
                with
                | ex ->
                    eprintfn "%A\n" ex
                    gamma

        go newGamma

    go TyEnv.empty

[<EntryPoint>]
let main argv =
    if argv.Length = 1 then
        repFile argv.[0]
    else
        repl ()

    0 // return an integer exit code
