open System
open System.IO
open Ch5.Parse

let parseAndPrint text =
    text |> ParserFacade.parse |> printfn "%A"

let repFile (filename: string) =
    use reader =
        new StreamReader(filename, Text.Encoding.UTF8)

    let rec go () =
        match reader.ReadLine() with
        | null -> ()
        | line ->
            parseAndPrint line
            go ()

    go ()

let repl () =
    let rec go () =
        printf "CoreML> "

        match Console.ReadLine() with
        | null -> ()
        | line ->
            try
                parseAndPrint line
            with
            | ex -> printfn "%A" ex

            go ()

    go ()

[<EntryPoint>]
let main argv =
    if argv.Length = 1 then
        repFile argv.[0]
    else
        repl ()

    0 // return an integer exit code
