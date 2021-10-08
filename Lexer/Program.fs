open System
open Lexer.Parser

module Lexer = 
    
    let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
    
    let rec lex input =
        match input with
        | [] -> []
        | head : string :: tail -> 
            match head.[head.Length-1].ToString() with
            | "+" | "*" | "-" | "^" ->  head :: lex tail
            | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> 
                    if head.Length > 0 && List.contains tail.[0] digits then ( 
                        if tail.Length > 1 then lex (head + tail.[0] :: tail.[1 ..])
                        else [head + tail.[0]])
                    else head :: lex tail
            | _ -> failwith "invalid value";;


[<EntryPoint>]
let main args=
    Console.WriteLine(sprintf "%A" (Lexer.lex ["1"; "*"; "1"; "0"; "0"; "+"; "1"; "2"; "3"; "4"; "1"; "2"; "1";]))
    Console.WriteLine(sprintf "%A" (Lexer.lex ["+"; "*"; "+"; "+"; "0"; "+"; "1"; "2"; "3"; "4"; "1"; "2"; "1";]))
    Console.WriteLine(sprintf "%A" (Lexer.lex ["1"; "*"; "1"; "0"; "0"; "+"; "1"; "2"; "3"; "4"; "1"; "2"; "+";]))
    
    let tokens = [Parser.Int(1); Parser.Plus; Parser.Int(2)]
    
    let res = Parser.expression tokens
    
    Console.WriteLine(sprintf "%A" res)
    
    0;;
   