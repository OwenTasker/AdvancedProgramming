/// <summary>
/// Module containing functions for parsing of statements passed to the MyMathsPal Interpreter. Represents acceptable
/// statements in Bachus-Naur Form as a set of functions.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module internal Interpreter.Parser

open Interpreter.Util

// https://www.itu.dk/~sestoft/parsernotes-fsharp.pdf

// statement ::= variable -> expression
/// <summary>Function to parse a statement</summary>
let rec private statement terminals =
    match terminals with
    | Word _ :: Assign :: tail -> expression tail
    | _ -> expression terminals

// expression ::= term expression' | function expression
and private expression terminals = (term >> expressionP) terminals

// expression' ::= + term expression' | function expression' | empty
and private expressionP terminals =
    match terminals with
    | Plus :: terminalsTail
    | Minus :: terminalsTail -> (term >> expressionP) terminalsTail
    | _ -> terminals

// term ::= exponent term'
and private term terminals = (exponent >> termP) terminals

// term' ::= * exponent term' | empty
and private termP terminals =
    match terminals with
    | Times :: terminalsTail
    | Divide :: terminalsTail -> (exponent >> termP) terminalsTail
    | _ -> terminals

// exponent ::= unary exponent'
and private exponent terminals = (unary >> exponentP) terminals

// exponent' ::= ^ unary exponent' | empty
and exponentP terminals =
    match terminals with
    | Exponent :: terminalsTail -> (unary >> exponentP) terminalsTail
    | _ -> terminals

// unary ::= -unary | +unary | factor
and private unary terminals =
    match terminals with
    | [UnaryMinus]
    | [UnaryPlus]
    | [] -> ParseError "Parse Error: Malformed Expression" |> raise
    | UnaryMinus :: terminalsTail
    | UnaryPlus :: terminalsTail -> unary terminalsTail
    | _ -> factor terminals

// factor ::= int | ( expression )
and private factor terminals =
    match terminals with
    | [] -> []
    | Number _ :: terminalsTail
    | Word _ :: terminalsTail ->
        match terminalsTail with
        | Number _ :: _
        | Word _ :: _ -> ParseError "Parse Error: Missing Operator" |> raise
        | [Plus]
        | [Minus]
        | [Times]
        | [Divide]
        | [Assign]
        | [Exponent] -> ParseError "Parse Error: Trailing Operator" |> raise
        | _ -> terminalsTail
    | Function _ :: Lpar :: terminalsTail -> arguments terminalsTail
    | Lpar :: terminalsTail ->
        match expression terminalsTail with
        | Rpar :: Number _ :: _
        | Rpar :: Word _ :: _ -> ParseError "Parse Error: Missing Operator" |> raise
        | Rpar :: terminalsTailTail -> terminalsTailTail
        | _ -> ParseError "Parse Error: Missing Right Parenthesis" |> raise
    | [UnaryMinus]
    | [UnaryPlus] -> ParseError "Parse Error: Trailing Operator" |> raise
    | _ -> ParseError "Parse Error: Malformed Expression" |> raise

and private arguments terminals =
    match terminals with
    | Word _ :: Assign :: tail -> expression tail |> arguments
    | Comma :: tail -> arguments tail
    | Number _ :: _
    | UnaryMinus :: _
    | UnaryPlus :: _
    | Word _ :: _ -> expression terminals |> arguments
    | Function _ :: _ -> factor terminals |> arguments
    | Lpar :: _ -> expression terminals |> arguments
    | Rpar :: tail -> tail
    | _ -> ParseError "Parse Error: Missing Right Parenthesis" |> raise

let internal parse terminals =
    if statement terminals = [] then terminals else ParseError "ParseError: Malformed expression." |> raise
