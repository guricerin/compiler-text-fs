module Parser.ParserFacade

open FSharp.Text.Lexing

let parse s =
    Parser.toplevel Lexer.main (LexBuffer<_>.FromString s)
