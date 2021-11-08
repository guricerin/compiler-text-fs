// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Ch2.TM

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    printfn "%A" TM.a
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code