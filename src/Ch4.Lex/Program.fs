open System
open System.IO
open FSharp.Text.Lexing
open Ch4.Lex

let top file =
    let stream =
        File.ReadAllLines(file)
        |> fun arr -> String.Join(" ", arr)
        |> String.Concat
        |> fun x -> x.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)

    for s in stream do
        s
        |> LexBuffer<_>.FromString
        |> Lexer.main
        |> fun tok -> tok.ToString()
        |> printfn "%s"

[<EntryPoint>]
let main argv =
    if argv.Length = 1 then
        top argv.[0]
    else
        ()

    0 // return an integer exit code
