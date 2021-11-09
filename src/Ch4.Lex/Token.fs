module Ch4.Lex.Token

// [<RequireQualifiedAccess>]
type Token =
    | EOF
    | UNDERBAR
    | ID of string
    | STRING of string
    | REAL of string
    | SPECIAL of string
    override this.ToString() =
        match this with
        | EOF -> "EOF "
        | ID s -> $"ID {s}"
        | REAL s -> $"REAL {s}"
        | STRING s -> $"STRING \"{s}\""
        | UNDERBAR -> "UNDERBAR "
        | SPECIAL s -> $"SPECIAL {s}"
