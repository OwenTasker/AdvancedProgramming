module Interpreter.Program

open System
open Interpreter.Lexer
open Interpreter.Parser

module Program =
    [<EntryPoint>]
    let main args =
        
        let rawInput = ["1"; "*"; "1"; "0"; "0"; "+"; "1"; "2"; "3"; "4"; "1"; "2"; "1";]
        let lexedVal = tokenize rawInput
        let scannedVals = scan lexedVal []
        let parsed = expression scannedVals
        
        Console.WriteLine (sprintf "%A" rawInput)
        Console.WriteLine (sprintf "%A" lexedVal)
        Console.WriteLine (sprintf "%A" scannedVals)
        Console.WriteLine (sprintf "%A" parsed)
        0