module Interpreter.Exec

open Interpreter.Util

let calculate operator (op1: float) (op2: float) =
    match operator with
    | terminal.Plus -> op1 + op2
    | terminal.Minus -> op1 - op2
    | terminal.Times -> op1 * op2
    | terminal.Divide -> op1 / op2
    | terminal.Exponent -> op1 ** op2
    | _ -> failwith "invalid operator"
            
        