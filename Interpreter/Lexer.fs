module Interpreter.Lexer

open System
open System.Text.RegularExpressions
open Interpreter.Util


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
        | "+" ->
            if output.Length > 0 then
                match output.[0] with
                    | Rpar 
                    | Number _ -> scan tokensTail (Plus :: output)
                    | _ -> scan tokensTail (UnaryPlus :: output)
            else scan tokensTail (UnaryPlus :: output)
        | "-" ->
            if tokensTail.Length > 0 then
                    match tokensTail.[0] with
                    | ">" -> scan tokensTail.Tail (Assign :: output)
                    | _ ->
                        if output.Length > 0 then
                           match output.[0] with
                           | Rpar
                           | Number _ -> scan tokensTail (Minus :: output)
                           | _ -> scan tokensTail (UnaryMinus :: output)
                        else scan tokensTail (UnaryMinus :: output)
            elif output.Length > 0 then 
                match output.[0] with 
                | Rpar 
                | Number _ -> scan tokensTail (Minus :: output)
                | _ -> scan tokensTail (UnaryMinus :: output)
            else scan tokensTail (UnaryMinus :: output)
        | "^" -> scan tokensTail (Exponent :: output)
        | "*" -> scan tokensTail (Times :: output)
        | "(" -> scan tokensTail (Lpar :: output)
        | ")" -> scan tokensTail (Rpar :: output)
        | "/" -> scan tokensTail (Divide :: output)
        | "=" -> scan tokensTail (Equals :: output)
        | FunctionMatch _ -> scan tokensTail (Function tokenHead :: output)
        | NumberMatchScan _ -> scan tokensTail (Number(Double.Parse tokenHead) :: output)
        | AlphabetMatch _ -> scan tokensTail (Word tokenHead :: output)
        
        | _ -> raise ScanError
        
let lexer input =
    let tokenizedVal = tokenize input
    let scannedInput = scan tokenizedVal []
    scannedInput
