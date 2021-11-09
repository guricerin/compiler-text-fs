module Ch4.Lex.ParserFacade

open FSharp.Text.Lexing

let parse s =
    Parser.toplevel Lexer.main (LexBuffer<_>.FromString s)
