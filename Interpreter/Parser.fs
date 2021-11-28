/// <summary>
/// Module containing functions for parsing of statements passed to the MyMathsPal Interpreter. Represents acceptable
/// statements in Bachus-Naur Form as a set of functions.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module Interpreter.Parser

open Interpreter.Util
    
// https://www.itu.dk/~sestoft/parsernotes-fsharp.pdf

// statement ::= variable -> expression
/// <summary>Function to parse a statement</summary>
let rec statement terminals =
    match terminals with
    | Word _ :: tail ->
        match tail with
        | Assign :: tailtail -> expression tailtail
        | _ -> expression terminals
    | _ -> expression terminals
    
// expression ::= term expression' | function expression
and expression terminals = (term >> expressionP) terminals

// expression' ::= + term expression' | function expression' | empty
and expressionP terminals =
    match terminals with
    | Plus :: terminalsTail
    | Minus :: terminalsTail -> (term >> expressionP) terminalsTail
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
    | [UnaryMinus]
    | [UnaryPlus]
    | [] -> ParseError "Parse Error: Malformed Expression" |> raise
    | UnaryMinus :: terminalsTail
    | UnaryPlus :: terminalsTail -> unary terminalsTail
    | _ -> factor terminals
    
// factor ::= int | ( expression ) 
and factor terminals =
    match terminals with
    | [] -> []
    | Number _ :: terminalsTail ->
        match terminalsTail with
        | Lpar :: _
        | Number _ :: _
        | Word _ :: _ ->  ParseError "Parse Error: Missing Operator" |> raise
        | [Plus]
        | [Minus]  
        | [Times]  
        | [Divide]
        | [Assign]
        | [Exponent] -> ParseError "Parse Error: Trailing Operator" |> raise
        | _ -> terminalsTail
    | Word _ :: terminalsTail ->
        match terminalsTail with
        | Lpar :: tailTail -> arguments tailTail
        | Number _ :: _
        | Word _ :: _ -> ParseError "Parse Error: Word Then Word Or Number not allowed" |> raise
        | [Plus]
        | [Minus]  
        | [Times]  
        | [Divide]
        | [Assign]
        | [Exponent] -> ParseError "Parse Error: Trailing Operator" |> raise
        | _ -> terminalsTail
    | Function _ :: Lpar :: terminalsTail ->
        match terminalsTail with
        | Word _ :: Assign :: _ -> arguments terminalsTail
        | _ ->
            match expression terminalsTail with
            | Rpar :: terminalsTail ->
                match terminalsTail with
                | Number _ :: _
                | Word _ :: _ -> ParseError "Parse Error: Missing Operator" |> raise
                | _ -> terminalsTail
            | Comma :: terminalsTailTail -> arguments terminalsTailTail
            | _ -> ParseError "Parse Error: Missing Right Parenthesis" |> raise
    | Lpar :: terminalsTail ->
        let x = expression terminalsTail
        match x with
        | Rpar :: terminalsTail ->
            match terminalsTail with
            | Number _ :: _
            | Word _ :: _ -> ParseError "Parse Error: Missing Operator" |> raise
            | _ -> terminalsTail
        | _ -> ParseError "Parse Error: Missing Right Parenthesis" |> raise
    | [UnaryMinus]
    | [UnaryPlus] -> ParseError "Parse Error: Trailing Operator" |> raise
    | _ -> ParseError "Parse Error: Malformed Expression" |> raise
    
and arguments terminals =
    match terminals with
    | Word _ :: Assign :: tail -> expression tail |> arguments 
    | Comma :: tail
    | Number _ :: tail
    | UnaryMinus :: tail
    | Word _ :: tail -> arguments tail
    | Function _ :: _ -> factor terminals |> arguments
    | Lpar :: _ -> expression terminals |> arguments
    | Rpar :: tail -> expressionP tail
    | _ -> ParseError "Parse Error: Missing Right Parenthesis" |> raise
     
let parse terminals =
    if statement terminals = [] then terminals else ParseError "ParseError: Malformed expression." |> raise
