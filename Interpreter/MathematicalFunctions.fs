﻿module Interpreter.MathematicalFunctions

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
        | _ -> LogELessThanOrEqualToZeroPointFive input (increment+1.0) sum+(((input-1.0)**increment)/increment)

let LogE input =
    if input <= 0.0 then
        raise (InvalidArgumentError "Input To LogE Can Not Be 0")
    elif input <= 0.5 then
        LogELessThanOrEqualToZeroPointFive input 1.0 0.0
    else
        LogEGreaterThanZeroPointFive input 1.0 0.0
        
let formNewBaseRuleFraction numerator denominator =
    try
        LogE numerator / LogE denominator
    with
        | :? InvalidArgumentError -> raise (InvalidArgumentError "Input to Logarithmic Function Cannot Be Less Than 0")

let Log2 input =
    formNewBaseRuleFraction input 2.0
    
let Log10 input =
    formNewBaseRuleFraction input 10.0
    
let LogX newBase input =
    formNewBaseRuleFraction input newBase
    
let TerminalLog logFunction logVal=
    match logFunction with
    | nameof(LogE) -> LogE logVal |> Number
    | nameof(Log10) -> Log10 logVal |> Number
    | nameof(Log2) -> Log2 logVal |> Number
    | _ -> InvalidArgumentError "Invalid Arguments" |> raise
    
let rootToTerminals (terminals: terminal list) denominator =
    [Lpar; Lpar] @ terminals @ [Rpar; Exponent; Lpar; Number 1.0; Divide; Number denominator; Rpar; Rpar]
    
let floorToNumber (numToFloor:float) =
    numToFloor |> int |> float
    
let ceilToNumber num =
    floorToNumber num + 1.0
    
let numToTerminal (num:float)=
    num |> Number