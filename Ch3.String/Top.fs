namespace Ch3.String

module Top =
    open System
    open System.IO

    let top file =
        let stream =
            File.ReadAllLines(file)
            |> fun arr -> String.Join(" ", arr)
            |> String.Concat
            |> fun x -> x.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)

        for s in stream do
            printfn "%s" s
