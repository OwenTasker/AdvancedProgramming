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

// statement ::= variable -> expression | expression
/// <summary>
/// Function to parse a statement, follows the BNF For our program to the letter
/// </summary>
///
/// <remarks>reference: https://www.itu.dk/~sestoft/parsernotes-fsharp.pdf</remarks>
///
/// <param name="terminals">A list of terminal values representing an input</param>
///
/// <returns>
/// Returns an empty list if parsing is complete, this means there are no dangling operators or similar
/// </returns>
let rec private statement terminals =
    match terminals with
    | Word _ :: Assign :: tail -> expression tail
    | _ -> expression terminals

// expression ::= term expression' 
and private expression terminals = (term >> expressionP) terminals

// expression' ::= + term expression' | - term expression' | empty
and private expressionP terminals =
    match terminals with
    | Plus :: terminalsTail
    | Minus :: terminalsTail -> (term >> expressionP) terminalsTail
    | _ -> terminals

// term ::= exponent term'
and private term terminals = (exponent >> termP) terminals

// term' ::= * factor term' | / factor term' | empty
and private termP terminals =
    match terminals with
    | Times :: terminalsTail
    | Divide :: terminalsTail -> (exponent >> termP) terminalsTail
    | _ -> terminals

// exponent ::= unary exponent'
and private exponent terminals = (unary >> exponentP) terminals

// exponent' ::= ^ unary exponent'
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

// factor ::= var | Number | Function (ARGS) | ( expression )
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

// arguments ::= var -> expression, arguments | expression, arguments 
and private arguments terminals =
    match terminals with
    | Word _ :: Assign :: tail -> expression tail |> arguments
    | Comma :: tail -> arguments tail
    | Number _ :: _
    | UnaryMinus :: _
    | UnaryPlus :: _
    | Word _ :: _ -> expression terminals |> arguments
    | Function _ :: _ -> expression terminals |> arguments
    | Lpar :: _ -> expression terminals |> arguments
    | Rpar :: tail -> tail
    | _ -> ParseError "Parse Error: Missing Right Parenthesis" |> raise

/// <summary>
/// Wrapper function for statement, if statement returns an empty list then the this will simply return the input,
/// otherwise it will throw a Parse Error, this should only occur when the terminal list was not fully popped during
/// the parsing process
/// </summary>
///
/// <param name="terminals">A list of terminal values representing an input</param>
///
/// <returns>
/// Returns the input given it is valid, will throw a ParseError whenever an invalid combination is recognized
/// </returns>
let internal parse terminals =
    if statement terminals = [] then terminals else ParseError "Parse Error: Malformed expression." |> raise
