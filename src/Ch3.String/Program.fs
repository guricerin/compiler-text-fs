open Ch3.String.Top

[<EntryPoint>]
let main argv =
    if argv.Length = 1 then
        top argv.[0]
    else
        ()

    0 // return an integer exit code
