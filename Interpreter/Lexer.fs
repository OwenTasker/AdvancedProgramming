module Interpreter.Lexer

open System
open Interpreter.Util
open System.Text.RegularExpressions

let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
let alphabet = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";
                "n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                "A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";
                "N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z"]
let functionRegexString =
    let functions = ["ceil";"floor";"sqrt";"round"]
    let functionRegex = [for i in functions -> "(^" + i + "$)|"]
    let generateRegex = (String.concat "" functionRegex)
    generateRegex.Remove(generateRegex.Length-1)

let symbolRegexString =
    let symbols = ["+";"*";"-";"^";"/";"=";"(";")"]
    let functionRegex = [for i in symbols -> "[\\" + i + "]|"]
    let generateRegex = (String.concat "" functionRegex)
    generateRegex.Remove(generateRegex.Length-1)
    
//https://sodocumentation.net/fsharp/topic/962/active-patterns
let (|AlphabetMatch|_|) (input:string)  =
    if Regex.IsMatch(input, "[a-zA-Z]+") then
        Some(input)
    else
        None
        
let (|SymbolMatch|_|) (input:string)  =
    if Regex.IsMatch(input, symbolRegexString) then
        Some(input)
    else
        None
        
let (|NumberMatchLex|_|) (input:string) =
    if Regex.IsMatch(input, "[0-9]+|[.]") then
        Some(input)
    else
        None
        
let (|FunctionMatch|_|) (input:string) =
    if Regex.IsMatch(input, functionRegexString) then
        Some(input)
    else
        None
        
let (|NumberMatchScan|_|) (input:string) =
    //https://stackoverflow.com/questions/12643009/regular-expression-for-floating-point-numbers
    if Regex.IsMatch(input, "[+-]?([0-9]*[.])?[0-9]+") then
        Some(input)
    else
        None
    
// Recursively lex the characters by calling lex at the head of the list and calling lex on the remaining
// elements.
// Build numbers/words by concatenating the individual chars into a single string and calling
// lex on the tail of the tail.
let rec tokenize input =
    match input with
    | [] | [""] -> []
    | head : string :: tail ->
        let h1 = head.[head.Length-1].ToString();
        match h1 with
        | " " -> tokenize tail
        | SymbolMatch _ ->  head :: tokenize tail
        | NumberMatchLex _ ->
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
        | _ -> raise TokenizeError
        
// Scan each token by recursively scanning the list tail. Prepend elements to output and
// reverse for efficiency.
let rec scan tokens output  =
    match tokens with
    | [] | [""] -> List.rev output
    | tokenHead :: tokensTail ->
        match tokenHead with
        | "+" -> scan tokensTail (Plus :: output)
        | "-" -> scan tokensTail (Minus :: output)
        | "^" -> scan tokensTail (Exponent :: output)
        | "*" -> scan tokensTail (Times :: output)
        | "(" -> scan tokensTail (Lpar :: output)
        | ")" -> scan tokensTail (Rpar :: output)
        | "/" -> scan tokensTail (Divide :: output)
        | "=" -> scan tokensTail (Equals :: output)
        | FunctionMatch _ -> scan tokensTail (Function tokenHead :: output)
        | NumberMatchScan _ -> scan tokensTail (Float(Double.Parse tokenHead) :: output)
        | AlphabetMatch _ -> scan tokensTail (Word tokenHead :: output)
        
        | _ -> raise ScanError
        
let lexer input =
    let tokenizedVal = tokenize input
    let scannedInput = scan tokenizedVal []
    (tokenizedVal, scannedInput)
