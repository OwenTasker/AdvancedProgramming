module Interpreter.Exec

open Interpreter.Util

let calculate operator op1 op2 =
    match operator with
    | terminal.Plus ->
        match op1, op2 with
        | Float i, Float j -> i + j
        | _, _ -> failwith "invalid value"
    | terminal.Minus ->
        match op1, op2 with
        | Float i, Float j -> i - j
        | _, _ -> failwith "invalid value"
    | terminal.Times ->
        match op1, op2 with
        | Float i, Float j -> i * j
        | _, _ -> failwith "invalid value"
    | terminal.Divide ->
        match op1, op2 with
        | Float i, Float j -> i / j
        | _, _ -> failwith "invalid value"
    | terminal.Exponent ->
        match op1, op2 with
        | Float i, Float j -> i ** j
        | _, _ -> failwith "invalid value"
    | _ -> failwith "invalid operator"
        