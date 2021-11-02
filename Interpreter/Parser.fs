﻿module Interpreter.Parser

open Interpreter.Util
    
// https://www.itu.dk/~sestoft/parsernotes-fsharp.pdf

// statement ::= variable -> expression
let rec statement terminals =
    match terminals with
    | Word _ :: tail ->
        match tail with
        | Assign :: tailtail -> expression tailtail
        | _ -> expression terminals
    | _ -> expression terminals
    
// expression ::= term expression' | 
and expression terminals =
    match terminals with
    | Function _ :: Lpar :: terminalsTail ->
        let x = expression terminalsTail
        match x with
        | Rpar :: terminalsTail -> expressionP terminalsTail
        | _ -> raise (ParseError "Parse Error: Missing Right Parenthesis") 
    | _ -> (term >> expressionP) terminals

// expression' ::= + term expression' | empty
and expressionP terminals =
    match terminals with
    | Plus :: terminalsTail
    | Minus :: terminalsTail ->
        match terminalsTail with
        | Function _ :: Lpar :: terminalsTail ->
            let x = (term >> expressionP) terminalsTail
            match x with
            |Rpar :: terminalsTail -> terminalsTail
            |_ -> raise (ParseError "Parse Error: Missing Right Parenthesis")
        | _ -> (term >> expressionP) terminalsTail
    | _ -> terminals
    
// term ::= exponent term'
and term terminals = (exponent >> termP) terminals

// term' ::= * exponent term' | empty
and termP terminals =
    match terminals with
    | Times :: terminalsTail 
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
    | Number _ :: terminalsTail
    | Word _ :: terminalsTail ->
        match terminalsTail with
        | Lpar :: _
        | Number _ :: _
        | Word _ :: _ ->  raise (ParseError "Parse Error: Missing Operator")
        | _ -> terminalsTail
    | Lpar :: terminalsTail ->
        match expression terminalsTail with
        | Rpar :: terminalsTail ->
            match terminalsTail with
            | Number _ :: _
            | Word _ :: _ -> raise (ParseError "Parse Error: Missing Operator")
            | _ -> terminalsTail
        | _ -> raise (ParseError "Parse Error: Missing Right Parenthesis")
    | _ -> raise (ParseError "Parse Error: Malformed Expression")
    
        
let parse terminals =
    try
        if statement terminals = [] then true else false
    with
    | ParseError _ -> false
