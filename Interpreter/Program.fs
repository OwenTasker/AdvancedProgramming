module Interpreter.Program

open Interpreter.Lexer
open Interpreter.Parser
open Interpreter.Exec

let public interpret input env =
    lexer input |> parse |> exec env

let public closed input env =
    lexer input |> closed env

let public getTerminalListFromString input =
    lexer input |> parse

[<EntryPoint>]
let main _ =
    0
