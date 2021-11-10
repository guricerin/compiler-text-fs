module Ch5.Parse.ParserFacade

open FSharp.Text.Lexing

let parse s =
    Parser.toplevel Lexer.main (LexBuffer<_>.FromString s)
