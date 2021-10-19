module Interpreter.Program

open System
open Interpreter.Util
open Interpreter.Lexer
open Interpreter.Parser
open Interpreter.Exec

module Program =
    [<EntryPoint>]
    let main args =
        
        let rawInput = ["1"; "*"; "1"; "0"; "0"; "+"; "1"; "2"; "3"; "4"; "1"; "2"; "1";]
        let lexedVal = tokenize rawInput
        let scannedVals = scan lexedVal []
        let parsed = expression scannedVals
        
        Console.WriteLine $"%A{rawInput}"
        Console.WriteLine $"%A{lexedVal}"
        Console.WriteLine $"%A{scannedVals}"
        Console.WriteLine $"%A{parsed}"
        Console.WriteLine (sprintf $"{reduce scannedVals}")
        
        0