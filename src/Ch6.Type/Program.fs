open System
open System.IO
open Parser
open TypeInf

let parseTyinfPrint text =
    let ast = text |> ParserFacade.parse
    printfn $"Parse Result:\n{ast}"
    TypeInf.typeinf ast
    printfn ""

let repFile (filename: string) =
    use reader =
        new StreamReader(filename, Text.Encoding.UTF8)

    let rec go () =
        match reader.ReadLine() with
        | null -> ()
        | line ->
            parseTyinfPrint line
            go ()

    go ()

let repl () =
    let rec go () =
        printf "CoreML> "

        match Console.ReadLine() with
        | null -> ()
        | line ->
            try
                parseTyinfPrint line
            with
            | ex -> eprintfn "%A\n" ex

            go ()

    go ()

[<EntryPoint>]
let main argv =
    if argv.Length = 1 then
        repFile argv.[0]
    else
        repl ()

    0 // return an integer exit code
