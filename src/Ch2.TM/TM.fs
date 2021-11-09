namespace Ch2.TM

module TM =
    /// テープヘッドの移動方向
    type D =
        | R
        | L

    /// シンボル
    type S =
        /// blank
        | B
        /// 1
        | I
        /// 0
        | O

    /// 状態
    type Q =
        /// move
        | M
        /// halt
        | H

    /// 状態遷移関数
    type DeltaEntry =
        { param: Q * S
          retval: Q * S * D }
        static member create (param: Q * S) (retval: Q * S * D) = { param = param; retval = retval }

    type Delta = DeltaEntry list

    type Program = Q * Delta

    type Tape =
        { rLeft: S list
          cur: S
          right: S list }
        static member create rLeft cur right =
            { rLeft = rLeft
              cur = cur
              right = right }

    let a = M
