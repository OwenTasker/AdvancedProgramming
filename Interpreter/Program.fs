﻿module Interpreter.Program

open Interpreter.Lexer
open Interpreter.Parser
open Interpreter.Exec

let public interpret input env =
    lexer input |> parse |> exec env

let public closed input env =
    lexer input |> closed env

[<EntryPoint>]
let main args =
    0
