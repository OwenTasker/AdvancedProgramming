module Interpreter.Program

open System
open Interpreter.Util
open Interpreter.Lexer
open Interpreter.Parser
open Interpreter.Exec
open Interpreter.Differentiate

let interpret (input) =
    try
        let lexedVals = lexer input
        let x = statement lexedVals
        Console.WriteLine x        
        let a, _ = exec lexedVals (Map<string, terminal list>[])
        Console.WriteLine a
    with
    | TokenizeError _ as e -> Console.WriteLine(e.Message)
    | ScanError _ as e -> Console.WriteLine(e.Message)
    | ParseError _ as e -> Console.WriteLine(e.Message)
    | ExecError _ as e -> Console.WriteLine(e.Message)
    
    
            

[<EntryPoint>]
let main args =
    
    let rawInput = ["x"; "^"; "2"]
    let lexedVal = tokenize rawInput
    let scannedVals = scan lexedVal []
    //let parsed = expression scannedVals
    
    Console.WriteLine $"%A{rawInput}"
    Console.WriteLine $"%A{lexedVal}"
    Console.WriteLine $"%A{scannedVals}"
    Console.WriteLine (sprintf $"{differentiate scannedVals}")
        
    0