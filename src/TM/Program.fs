open TM

[<EntryPoint>]
let main argv =
    let p: Program =
        (M,
         [ DeltaEntry.create (M, I) (M, O, L)
           DeltaEntry.create (M, O) (H, I, L)
           DeltaEntry.create (M, B) (H, I, L) ])

    let t = Tape.create [ I; I; I ] I []
    let r = Eval.eval p t
    printfn "%A" t
    printfn "%A" r
    0 // return an integer exit code
