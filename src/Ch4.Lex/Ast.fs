module Ch4.Lex.Ast

type TokenKind =
    | Underbar
    | Id of string
    | Real of string
    | TString of string
    | Special of string

type Ast = Tokens of TokenKind list
