module Interpreter.Program

open System
open Interpreter.Lexer

module Program =
    [<EntryPoint>]
    let main args =
        
        let rawInput = ["1"; "*"; "1"; "0"; "0"; "+"; "1"; "2"; "3"; "4"; "1"; "2"; "1";]
        let lexedVal = Lexer.lex rawInput
        let scannedVals = Lexer.scan lexedVal []
        
        Console.WriteLine (sprintf "%A" rawInput)
        Console.WriteLine (sprintf "%A" lexedVal)
        Console.WriteLine (sprintf "%A" scannedVals)
        0