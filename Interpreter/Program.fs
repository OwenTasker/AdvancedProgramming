module Interpreter.Program

open System
open Interpreter.Util
open Interpreter.Lexer
open Interpreter.Parser
open Interpreter.Exec
open Interpreter.Differentiate

let interpret input env =
    lexer input |> parse |> exec env

let closed input env =
    lexer input |> closed env

[<EntryPoint>]
let main args =
    
//    let rawInput = ["x"; "^"; "2"; "+"; "x"; "^"; "2"]
//    let lexedVal = tokenize rawInput
//    let scannedVals = scan lexedVal []
//    //let parsed = expression scannedVals
//    
//    Console.WriteLine $"%A{rawInput}"
//    Console.WriteLine $"%A{lexedVal}"
//    Console.WriteLine $"%A{scannedVals}"
//    Console.WriteLine $"%A{differentiate scannedVals}"
    Console.WriteLine $"%A{countCommaOccurance [Comma;Comma;Rpar;Comma] 0}"
        
    0
