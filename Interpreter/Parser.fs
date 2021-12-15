/// <summary>
/// Module containing functions for parsing of statements passed to the MyMathsPal Interpreter. Represents acceptable
/// statements in Bachus-Naur Form as a set of functions.
/// </summary>
///
/// <remarks>reference: https://www.itu.dk/~sestoft/parsernotes-fsharp.pdf</remarks>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module internal Interpreter.Parser

open Interpreter.Util


/// <summary>
/// Function to parse a statement, expresses the following BNF statement
/// statement ::= variable -> expression | expression
/// </summary>
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

/// <summary>
/// Function to parse an expression, expresses the following BNF statement
/// expression ::= term expression'
/// </summary>
///
/// <param name="terminals">A list of terminal values representing an input</param>
///
/// <returns>
/// Returns an empty list if parsing is complete, this means there are no dangling operators or similar
/// </returns>
and private expression terminals = (term >> expressionP) terminals

/// <summary>
/// Function to parse an expression', expresses the following BNF statement
/// expression' ::= + term expression' | - term expression' | empty
/// </summary>
///
/// <param name="terminals">A list of terminal values representing an input</param>
///
/// <returns>
/// Returns an empty list if parsing is complete, this means there are no dangling operators or similar
/// </returns>
and private expressionP terminals =
    match terminals with
    | Plus :: terminalsTail
    | Minus :: terminalsTail -> (term >> expressionP) terminalsTail
    | _ -> terminals

/// <summary>
/// Function to parse a term, expresses the following BNF statement
/// term ::= exponent term'
/// </summary>
///
/// <param name="terminals">A list of terminal values representing an input</param>
///
/// <returns>
/// Returns an empty list if parsing is complete, this means there are no dangling operators or similar
/// </returns>
and private term terminals = (exponent >> termP) terminals

/// <summary>
/// Function to parse a term', expresses the following BNF statement
/// term' ::= * exponent term' | / exponent term' | empty
/// </summary>
///
/// <param name="terminals">A list of terminal values representing an input</param>
///
/// <returns>
/// Returns an empty list if parsing is complete, this means there are no dangling operators or similar
/// </returns>
and private termP terminals =
    match terminals with
    | Times :: terminalsTail
    | Divide :: terminalsTail -> (exponent >> termP) terminalsTail
    | _ -> terminals

/// <summary>
/// Function to parse an exponeent, expresses the following BNF statement
/// exponent ::= unary exponent'
/// </summary>
///
/// <param name="terminals">A list of terminal values representing an input</param>
///
/// <returns>
/// Returns an empty list if parsing is complete, this means there are no dangling operators or similar
/// </returns>
and private exponent terminals = (unary >> exponentP) terminals

/// <summary>
/// Function to parse a statement, expresses the following BNF statement
/// exponent' ::= ^ unary exponent' | empty
/// </summary>
///
/// <param name="terminals">A list of terminal values representing an input</param>
///
/// <returns>
/// Returns an empty list if parsing is complete, this means there are no dangling operators or similar
/// </returns>
and exponentP terminals =
    match terminals with
    | Exponent :: terminalsTail -> (unary >> exponentP) terminalsTail
    | _ -> terminals

/// <summary>
/// Function to parse a unary, expresses the following BNF statement
/// unary ::= -unary | +unary | factor | empty
/// </summary>
///
/// <param name="terminals">A list of terminal values representing an input</param>
///
/// <returns>
/// Returns an empty list if parsing is complete, this means there are no dangling operators or similar
/// </returns>
and private unary terminals =
    match terminals with
    | [UnaryMinus]
    | [UnaryPlus]
    | [] -> ParseError "Parse Error: Empty expression at unary BNF state" |> raise
    | UnaryMinus :: terminalsTail
    | UnaryPlus :: terminalsTail -> unary terminalsTail
    | _ -> factor terminals

/// <summary>
/// Function to parse a factor, expresses the following BNF statement
/// factor ::= var | Number | Function (ARGS) | ( expression )
/// </summary>
///
/// <param name="terminals">A list of terminal values representing an input</param>
///
/// <returns>
/// Returns an empty list if parsing is complete, this means there are no dangling operators or similar
/// </returns>
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
    | a -> ParseError ("Parse Error: Non-factor expression passed to factor BNF state \"" + (terminalListToString "" a) + "\"" ) |> raise

/// <summary>
/// Function to parse arguments, expresses the following BNF statement
/// arguments ::= var -> expression, arguments | expression, arguments
/// </summary>
///
/// <param name="terminals">A list of terminal values representing an input</param>
///
/// <returns>
/// Returns an empty list if parsing is complete, this means there are no dangling operators or similar
/// </returns>
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
