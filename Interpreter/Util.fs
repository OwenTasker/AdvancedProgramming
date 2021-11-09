/// <summary>
/// Module containing helper methods used by various areas of the MyMathsPal Interpreter.
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
                 ]


/// <summary>List of valid digits in the Interpreter. To be used in tokenizing input.</summary>

// https://stackoverflow.com/questions/42253284/f-check-if-a-string-contains-only-number
/// <summary>Function to test whether a string is comprised only of digits.</summary>
let strContainsOnlyNumber (s:string) = System.Double.TryParse s |> fst

//https://gist.github.com/theburningmonk/3363893
/// <summary>Function to convert a C# Dictionary to an F# Map.</summary>
let inline toMap kvps =
    kvps
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

/// <summary>Function to convert a terminal to its string representation.</summary>
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
let rec terminalListToString str list =
    match list with
    | head :: tail -> terminalListToString (str + individualTerminalToString head ) tail
    | [] -> str
