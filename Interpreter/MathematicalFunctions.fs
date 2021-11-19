module Interpreter.MathematicalFunctions

open System
open Interpreter.Util

//Reference :: https://www.efunda.com/math/taylor_series/logarithmic.cfm
let rec LogEGreaterThanZeroPointFive (input:float) (increment:float) (sum:float) =
    match increment with
    | 2000.0 -> sum
    | _  -> LogEGreaterThanZeroPointFive input (increment+1.0) (sum+(1.0/increment)*((input-1.0)/input)**increment)

let rec LogELessThanOrEqualToZeroPointFive (input:float) (increment:float) (sum:float) =
    match increment with
    | 2000.0 -> sum
    | _ ->
        match (increment%2.0) with
        | 0.0 -> LogELessThanOrEqualToZeroPointFive input (increment+1.0) sum-(((input-1.0)**increment)/increment)
        | 1.0 -> LogELessThanOrEqualToZeroPointFive input (increment+1.0) sum+(((input-1.0)**increment)/increment)
        | _ -> raise (InvalidArgumentError "Invalid Argument Error: Increment Not A Whole Number")

let LogE' input =
    if input <= 0.0 then
        raise (InvalidArgumentError "Input To LogE Can Not Be 0")
    elif input <= 0.5 then
        LogELessThanOrEqualToZeroPointFive input 1.0 0.0
    else
        LogEGreaterThanZeroPointFive input 1.0 0.0
        
let formNewBaseRuleFraction numerator denominator =
    LogE' numerator / LogE' denominator

let Log2 input =
    formNewBaseRuleFraction input 2.0
    
let Log10 input =
    formNewBaseRuleFraction input 10.0
    
let LogX newBase input =
    formNewBaseRuleFraction input newBase