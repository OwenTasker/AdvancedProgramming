/// <summary>
/// Module containing functions for lexing characters passed to the MyMathsPal Interpreter.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module Interpreter.Lexer

open System
open System.Text.RegularExpressions
open Interpreter.Util

/// <summary>List of valid digits in the Interpreter. To be used in tokenizing input.</summary>
let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]

/// <summary>List of valid letters in the Interpreter. To be used in tokenizing input.</summary>
let alphabet = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";
                "n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                "A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";
                "N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z"]

/// <summary>Regex string for matching predefined functions.</summary>
let functionRegexString =
    let functionRegex = [
        for x,_ in functions -> "(^" + x + "$)|"
    ]
    let generateRegex = (String.concat "" functionRegex)
    generateRegex.Remove(generateRegex.Length-1)
    
/// <summary>Regex string for matching acceptable symbols.</summary>
let symbolRegexString =
    let symbols = ["+";"*";"-";"^";"/";"=";"(";")";">";","]
    let symbolRegex = [
        for i in symbols -> "(^\\" + i + "$)|"
    ]
    let generateRegex = (String.concat "" symbolRegex)
    generateRegex.Remove(generateRegex.Length-1)

//https://sodocumentation.net/fsharp/topic/962/active-patterns
/// <summary>Function to match valid number characters passed to tokenize.</summary>
///
/// <param name="input">A character as a string.</param>
///
/// <returns>An Option containing the input string if it was a number or a period, or None otherwise.</returns>
let (|IntegerOrPeriodMatch|_|) (input:string) =
    if Regex.IsMatch(input, "[0-9]+|[.]") then
        Some(input)
    else
        None

//https://stackoverflow.com/questions/12643009/regular-expression-for-floating-point-numbers
/// <summary>Function to match numbers passed to scan.</summary>
///
/// <param name="input">A string to query for its composition.</param>
///
/// <returns>An Option containing the input string if it was a number, or None otherwise.</returns> 
let (|IntegerOrFloatMatch|_|) (input:string) =
    if Regex.IsMatch(input, "([0-9]*[.])?[0-9]+") then
        Some(input)
    else
        None

/// <summary>Function to match letters passed to the passed to tokenize or scan.</summary>
/// 
///  <param name="input">A string to query for its composition.</param>
///
/// <returns>An Option containing the input string if it was alphabetic, or None otherwise.</returns> 
let (|AlphabetMatch|_|) (input:string)  =
    if Regex.IsMatch(input, "[a-zA-Z]+") then
        Some(input)
    else
        None

/// <summary>Function to match symbols passed to the passed to tokenize or scan.</summary>
/// 
/// <param name="input">A string to query for its composition.</param>
///
/// <returns>An Option containing the input string if it was a recognised symbol, or None otherwise.</returns>
let (|SymbolMatch|_|) (input:string)  =
    if Regex.IsMatch(input, symbolRegexString) then
        Some(input)
    else
        None

/// <summary>Function to match functions passed to the passed to tokenize or scan.</summary>
///
/// <param name="input">A string to query for its composition.</param>
///
/// <returns>An Option containing the input string if it was a predefined function, or None otherwise.</returns>
let (|FunctionMatch|_|) (input:string) =
    if Regex.IsMatch(input, functionRegexString) then
        Some(input)
    else
        None

/// <summary>
/// Function to tokenize characters passed as a list of the string representations recursively by processing the head
/// element and calling again on the tail. Builds numbers and words by compiling into the next element to tokenize
/// until it is not a number or letter respectively.
/// </summary>
///
/// <param name="input">A list of characters as strings.</param>
///
/// <returns>A list of valid tokens.</returns>
let rec tokenize input =
    match input with
    | [] | [""] -> []
    | head : string :: tail ->
        let h1 = head.[head.Length-1].ToString();
        match h1 with
        | " " -> tokenize tail
        | "-" ->
            match tail with
            | ">" :: tailTail -> head + ">" :: tokenize tailTail
            | _ -> head :: tokenize tail
        | "=" -> raise (TokenizeError "Invalid Operator. The Assignment Operator is \"->\"")
        | SymbolMatch _ ->  head :: tokenize tail
        | IntegerOrPeriodMatch _ ->
            if tail.Length > 0 then(
            // If we already have a number in head and the first tail element is a digit
                if head.Length >= 1 && (List.contains tail.[0] digits || tail.[0] = ".") then (
                    // If the tail has further elements to lex after the digit
                    // then append the digit to the number being built and lex the remaining characters
                    if tail.Length > 1 then
                        tokenize (head + tail.[0] :: tail.[1 ..])
                    // else append the digit and don't call lex
                    else [head + tail.[0]])
                    // Build single digit number, lex next element
                else head :: tokenize tail
            )else [head]
        | AlphabetMatch _ ->
            if tail.Length > 0 then(
            // If we already have a letter in head and the first tail element is a letter
                if head.Length >= 1 && (List.contains tail.[0] alphabet) then (
                    // If the tail has further elements to lex after the digit
                    // then append the digit to the number being built and lex the remaining characters
                    if tail.Length > 1 then
                        tokenize (head + tail.[0] :: tail.[1 ..])
                    // else append the digit and don't call lex
                    else [head + tail.[0]])
                    // Build single digit number, lex next element
                else head :: tokenize tail
            )else [head]
        | _ -> raise (TokenizeError "Tokenize Error: Invalid Token recognized")

/// <summary>Function to read a list of tokens and output a list of equivalent terminals.</summary>
///
/// <param name="tokens">A list of tokens as strings.</param>
/// <param name="output">A list of terminals so far read from the input list</param>
///
/// <returns>A list of terminals.</returns>
let rec scan tokens output  =
    match tokens with
    | [] | [""] -> List.rev output
    | tokenHead :: tokensTail ->
        match tokenHead with
        | "+" ->
            if output.Length > 0 then
                match output.[0] with
                    | Rpar
                    | Word _
                    | Number _ -> scan tokensTail (Plus :: output)
                    | _ -> scan tokensTail (UnaryPlus :: output)
            else scan tokensTail (UnaryPlus :: output)
        | "-" ->
            if output.Length > 0 then
               match output.[0] with
               | Rpar
               | Word _
               | Number _ -> scan tokensTail (Minus :: output)
               | _ -> scan tokensTail (UnaryMinus :: output)
            else scan tokensTail (UnaryMinus :: output)
        | "^" -> scan tokensTail (Exponent :: output)
        | "*" -> scan tokensTail (Times :: output)
        | "(" -> scan tokensTail (Lpar :: output)
        | ")" -> scan tokensTail (Rpar :: output)
        | "/" -> scan tokensTail (Divide :: output)
        | "->" -> scan tokensTail (Assign :: output)
        | "," -> scan tokensTail (Comma :: output)
        | FunctionMatch _ -> scan tokensTail (Function tokenHead :: output)
        | IntegerOrFloatMatch _ -> scan tokensTail (Number(Double.Parse tokenHead) :: output)
        | AlphabetMatch _ -> scan tokensTail (Word tokenHead :: output)
        | _ -> raise (ScanError "Scan Error: Malformed Tokens") 

/// <summary>Function to lex an input list of characters as strings by calling tokenize then scan.</summary>
///
/// <param name="input">A list of characters as strings.</param>
///
/// <returns>A list of terminals.</returns>
let lexer input =
    scan (tokenize input) []
