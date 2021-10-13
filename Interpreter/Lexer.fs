module Interpreter.Lexer

open System
open Interpreter.Util

let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
    
// Recursively lex the characters by calling lex at the head of the list and calling lex on the remaining
// elements.
// Build numbers by concatenating the individual chars into a single string and calling lex on the tail of the tail.
let rec lex input =
    match input with
    | [] | [""] -> []
    | head : string :: tail ->
        // Match first string char as numbers can contain multiple characters and so will match with _
        // Perhaps change number matching to be generic rather than digit based.
        match head.[head.Length-1].ToString() with
        | "+" | "*" | "-" | "^" | "/" | "="->  head :: lex tail
        | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "." ->
            if tail.Length > 0 then(
            // If we already have a number in head and the first tail element is a digit
                if head.Length >= 1 && (List.contains tail.[0] digits || tail.[0] = ".") then (
                    // If the tail has further elements to lex after the digit
                    // then append the digit to the number being built and lex the remaining characters
                    if tail.Length > 1 then lex (head + tail.[0] :: tail.[1 ..])
                    // else append the digit and don't call lex
                    else [head + tail.[0]])
                    // Build single digit number, lex next element
                else head :: lex tail
                )else [head]
        | _ -> failwith "invalid value";;

    
// Scan each token by recursively scanning the list tail. Prepend elements to output and reverse for efficiency.
let rec scan tokens output  =
    match tokens with
    | [] -> List.rev output
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
        | _ ->
            if strContainsOnlyNumber(tokenHead) then
                match Int32.TryParse tokenHead with
                | true, i -> scan tokensTail (Int(i) :: output)
                | _ -> scan tokensTail (Float(Double.Parse tokenHead) :: output)
            else raise Scanerror    
