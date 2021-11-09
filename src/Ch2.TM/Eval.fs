namespace Ch2.TM

module Eval =
    open TM

    let moveL tape =
        let rLeft, cur =
            match tape.rLeft with
            | [] -> [], TM.B
            | l :: ls -> ls, l

        let right = tape.cur :: tape.right

        TM.Tape.create rLeft cur right

    let moveR tape =
        let rLeft = tape.cur :: tape.rLeft

        let cur, right =
            match tape.right with
            | [] -> TM.B, []
            | r :: rs -> r, rs

        TM.Tape.create rLeft cur right

    let move (direction: TM.D) (tape: TM.Tape) : TM.Tape =
        match direction with
        | TM.L -> moveL tape
        | TM.R -> moveR tape

    let rec exec (delta: TM.Delta) (q: TM.Q) (tape: TM.Tape) : TM.Tape =
        match List.tryFind (fun entry -> entry.param = (q, tape.cur)) delta with
        | None -> tape
        | Some { param = param; retval = (q', s, d) } -> exec delta q' (move d { tape with cur = s })

    let eval (p: TM.Program) (tape: TM.Tape) : TM.Tape =
        match p with
        | (state, delta) -> exec delta state tape
