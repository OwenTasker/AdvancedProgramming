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
    
// term ::= exponent term'
and term terminals = (exponent >> termP) terminals

// term' ::= * exponent term' | empty
and termP terminals =
    match terminals with
    | Times :: terminalsTail -> (exponent >> termP) terminalsTail
    | Divide :: terminalsTail -> (exponent >> termP) terminalsTail
    | _ -> terminals

// exponent ::= factor exponent'
and exponent terminals = (factor >> exponentP) terminals

// exponent' ::= ^ factor exponent' | empty
and exponentP terminals =
    match terminals with
    | Exponent :: terminalsTail -> (factor >> exponentP) terminalsTail
    | _ -> terminals

// factor ::= int | ( expression )
// Here the and keyword has allowed us to define the function with mutual recursion - an expression contains a
// factor and a factor contains an expression
and factor terminals =
    match terminals with
    | Float _ :: terminalsTail ->
        match terminalsTail with
        | Lpar :: tailTail -> raise ParseError
        | _ -> terminalsTail
    | Lpar :: terminalsTail ->
        match expression terminalsTail with
        | Rpar :: terminalsTail ->
            match terminalsTail with
            | Float _ :: tailTail -> raise ParseError
            | _ -> terminalsTail
        | _ -> raise ParseError
    | _ -> raise ParseError
    
        
