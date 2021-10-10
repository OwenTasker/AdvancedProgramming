open System
open Lexer.Parser
open Lexer.Parser.Parser

module Lexer = 
    
    let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
    
    // Recursively lex the characters by calling lex at the head of the list and calling lex on the remaining
    // elements.
    // Build numbers by concatenating the individual chars into a single string and calling lex on the tail of the tail.
    let rec lex input =
        match input with
        | [] -> []
        | head : string :: tail ->
            // Match first string char as numbers can contain multiple characters and so will match with _
            // Perhaps change number matching to be generic rather than digit based.
            match head.[head.Length-1].ToString() with
            | "+" | "*" | "-" | "^" ->  head :: lex tail
            | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
                    // If we already have a number in head and the first tail element is a digit
                    if head.Length > 0 && List.contains tail.[0] digits then (
                        // If the tail has further elements to lex after the digit
                        // then append the digit to the number being built and lex the remaining characters
                        if tail.Length > 1 then lex (head + tail.[0] :: tail.[1 ..])
                        // else append the digit and don't call lex
                        else [head + tail.[0]])
                    // Build single digit number, lex next element
                    else head :: lex tail
            | _ -> failwith "invalid value";;


[<EntryPoint>]
let main args=
    Console.WriteLine(sprintf "%A" (Lexer.lex ["1"; "*"; "1"; "0"; "0"; "+"; "1"; "2"; "3"; "4"; "1"; "2"; "1";]))
    Console.WriteLine(sprintf "%A" (Lexer.lex ["+"; "*"; "+"; "+"; "0"; "+"; "1"; "2"; "3"; "4"; "1"; "2"; "1";]))
    Console.WriteLine(sprintf "%A" (Lexer.lex ["1"; "*"; "1"; "0"; "0"; "+"; "1"; "2"; "3"; "4"; "1"; "2"; "+";]))
    
    let chars = ["1"; "*"; "1"; "0"; "0"; "+"; "1"; "2"; "3"; "4"; "1"; "2"; "1";]
    
    // Expect ["1"; "*", "100"; "+"; "1234121";]
    let tokens = Lexer.lex chars
    Console.WriteLine(sprintf "%A" tokens)
    
    // Expect [Int 1; Times, Int 100; Plus; Int 1234121;]
    let terminals = scan tokens []
    Console.WriteLine(sprintf "%A" terminals)
    
    // Expect []
    let expr = Parser.expression terminals
    Console.WriteLine(sprintf "%A" expr)
    
    0;;
   