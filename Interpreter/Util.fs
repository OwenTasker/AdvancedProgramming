/// <summary>
/// Module containing helper functions used by various areas of the MyMathsPal Interpreter.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module Interpreter.Util

/// <summary>Exception thrown when an error is encountered while parsing a list of terminals.</summary>
exception ParseError of string
/// <summary>Exception thrown when an error is encountered while scanning a list of tokens.</summary>
exception ScanError of string
/// <summary>Exception thrown when an error is encountered while tokenizing a list of strings.</summary>
exception TokenizeError of string
/// <summary>Exception thrown when an error is encountered while computing a binary operation.</summary>
exception CalculateError of string
/// <summary>Exception thrown when an error is encountered while computing a unary operation.</summary>
exception UnaryError of string
/// <summary>Exception thrown when an error is encountered while executing an expression.</summary>
exception ExecError of string
/// <summary>Exception thrown when an error is encountered while executing a function, used specifically for enforcing
/// more stringent parameters</summary>
exception InvalidArgumentError of string

/// <summary>A type representing terminal characters accepted by the Interpreter.</summary>
type terminal =
    | Plus
    | Times
    | Divide
    | Minus
    | UnaryPlus
    | UnaryMinus
    | Exponent
    | Lpar
    | Rpar
    | Assign
    | Comma    
    | Function of string
    | Word of string
    | Number of float
    
/// <summary>List of valid predefined functions in the Interpreter.</summary>
let functions = [
                 ("ceil", "One Argument; A function to determine the ceiling of a decimal value, given a value of 2.1, will return 3")
                 ("floor", "One Argument; A function to determine the floor of a decimal value, given a value of 2.1, will return 2")
                 ("sqrt", "One Argument; A function to determine the square root of a value, given a value of 4, will return 2")
                 ("cbrt", "One Argument; A function to determine the cube root of a value, given a value of 8, will return 2")
                 ("round", "One Argument; A function to determine the rounded value of the provided argument, given a value of 2.5, will return 3")
                 ("plot", "")
                 ("ln", "One Argument; A function to determine the natural logarithm of the provided argument to 6 accurate decimal points")
                 ("logTwo", "One Argument; A function to determine the base 2 logarithm of the provided argument to 6 accurate decimal points")
                 ("logTen", "One Argument; A function to determine the base 10 logarithm of the provided argument to 6 accurate decimal points")
                 ("logX", "Two Arguments; A function to determine the base X logarithm of the second provided argument to 6 accurate decimal points")
                 ("differentiate", "One Argument; A function to differentiate an expression provided as an argument, given a value of x^2, will return 2*x")
                 ]

//https://gist.github.com/theburningmonk/3363893
/// <summary>Function to convert a C# Dictionary to an F# Map.</summary>
let inline toMap kvps =
    kvps
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

/// <summary>Function to convert a terminal to its string representation.</summary>
///
/// <param name="x">A terminal represented by the terminal type.</param>
///
/// <returns>A string representation of the terminal.</returns>
let individualTerminalToString x =
    match x with
    | Times -> "*"
    | Divide -> "/"
    | Plus 
    | UnaryPlus -> "+"
    | Minus 
    | UnaryMinus -> "-"
    | Exponent -> "^"
    | Lpar -> "("
    | Rpar -> ")"
    | Assign -> "->"
    | Comma -> ","
    | Function func -> string func
    | Word word -> string word
    | Number num -> string num

/// <summary>Function to convert a list of terminals to a string.</summary>
///
/// <param name="str">A string containing the as yet converted terminals.</param>
/// <param name="list">A list of terminals to convert to a string.</param>
/// 
/// <returns>A string representation of the terminal list.</returns>
let rec terminalListToString str list =
    match list with
    | head :: tail -> terminalListToString (str + individualTerminalToString head ) tail
    | [] -> str
    
/// <summary>
/// Map containing the precedence and associativity of operators accepted by the performUnaryOperation and
/// performBinaryOperation functions.
/// </summary>
let precedenceAssociativityMap =
    Map [(UnaryMinus, (4, "r"))
         (UnaryPlus, (4, "r"))
         (Exponent, (3, "r"))
         (Times, (2, "l"))
         (Divide, (2, "l"))
         (Plus, (1, "l"))
         (Minus, (1, "l"))]

/// <summary>
/// Retrieves the precedence for an operator from the map.
/// </summary>
/// 
/// <param name="operator">A terminal representing an operator.</param>
///
/// <returns>The precedence value of the operator.</returns>
let getPrecedence operator =
    (Map.find operator precedenceAssociativityMap) |> fst

/// <summary>
/// Retrieves the associativity for an operator from the map.
/// </summary>
///
/// <param name="operator">A terminal representing an operator.</param>
///
/// <returns>The associativity value of the operator.</returns>
let getAssociativity operator =
    (Map.find operator precedenceAssociativityMap) |> snd
