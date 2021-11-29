module public Interpreter.PublicInterface

open Interpreter.Lexer
open Interpreter.Parser
open Interpreter.Exec

let public interpret input env =
    lexer input |> parse |> exec env

let public closed input env =
    lexer input |> closed env

let public stringToTerminalList input =
    lexer input |> parse

let public terminalListToString input =
    Util.terminalListToString "" input

let public toMap kvps =
    Util.toMap kvps