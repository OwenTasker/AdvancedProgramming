module Interpreter.Parser

open Interpreter.Util
    
// https://www.itu.dk/~sestoft/parsernotes-fsharp.pdf
    
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

// exponent ::= unary exponent'
and exponent terminals = (unary >> exponentP) terminals

// exponent' ::= ^ unary exponent' | empty
and exponentP terminals =
    match terminals with
    | Exponent :: terminalsTail -> (unary >> exponentP) terminalsTail
    | _ -> terminals
    
// unary ::= -unary | +unary | factor
and unary terminals =
    match terminals with
    | UnaryMinus :: terminalsTail
    | UnaryPlus :: terminalsTail -> unary terminalsTail
    | _ -> factor terminals

// factor ::= int | ( expression )
and factor terminals =
    match terminals with
    | Number _ :: terminalsTail ->
        match terminalsTail with
        | Lpar :: _ -> raise ParseError
        | _ -> terminalsTail
    | Lpar :: terminalsTail ->
        match expression terminalsTail with
        | Rpar :: terminalsTail ->
            match terminalsTail with
            | Number _ :: _ -> raise ParseError
            | _ -> terminalsTail
        | _ -> raise ParseError
    | _ -> raise ParseError
    
        
let parse terminals =
    try
        if expression terminals = [] then true else false
    with
    | ParseError -> false
