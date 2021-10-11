module Interpreter.Parser

open Interpreter.Util
    
// https://www.itu.dk/~sestoft/parsernotes-fsharp.pdf
    
// BNF defined recursively
// expression ::= term expression'
let rec expression terminals = (term >> expressionP) terminals

// expression' ::= + term expression' | empty
and expressionP terminals =
    match terminals with
    | Plus :: terminalsTail -> (term >> expressionP) terminalsTail
    | Minus :: terminalsTail -> (term >> expressionP) terminalsTail
    | _ -> terminals
    
// term ::= factor term'
and term terminals = (factor >> termP) terminals

// term' ::= * factor term' | empty
and termP terminals =
    match terminals with
    | Times :: terminalsTail -> (factor >> termP) terminalsTail
    | _ -> terminals

// factor ::= int | ( expression )
// Here the and keyword has allowed us to define the function with mutual recursion - an expression contains a
// factor and a factor contains an expression
and factor terminals =
    match terminals with
    | Int _ :: terminalsTail -> exponent terminalsTail
    | Float _ :: terminalsTail -> exponent terminalsTail
    | Lpar :: terminalsTail ->
        match expression terminalsTail with
        | Rpar :: terminalsTail -> exponent terminalsTail
        | _ -> raise Parseerror
    | _ -> raise Parseerror
    
and exponent terminals =
    match terminals with
    | Exponent :: terminalsTail -> factor terminalsTail
    | _ -> terminals
        
