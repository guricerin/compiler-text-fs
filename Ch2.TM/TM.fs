namespace Ch2.TM

module TM =
    type D = R | L

    type S = B | I | O

    /// 状態
    type Q = M | H

    /// 状態遷移関数
    type Delta = ((Q * S) * (Q * S * D)) list

    type Program = Q * Delta

    type Tape = S list * S * S list

    let a = M
