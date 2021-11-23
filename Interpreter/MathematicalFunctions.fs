﻿/// <summary>
/// Module containing all bespoke mathematical functions used, done to avoid using inbuilt libraries
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter.MathematicalFunctions</summary>
/// </namespacedoc>
module Interpreter.MathematicalFunctions

open Interpreter.Util

/// <summary>
/// Implements a Taylor-Series in order to approximate a value for LogE
/// </summary>
///
/// <remarks>
/// Reference :: https://www.efunda.com/math/taylor_series/logarithmic.cfm
/// </remarks>
///
/// <param name="input">A floating point value to take the logarithm base E of</param>
/// <param name="increment">
/// Current iteration of the taylor series, the more iterations performed, the more accurate
/// the approximation
/// </param>
/// <param name="sum">the cumulative sum of the Taylor Series</param>
///
/// <returns>The natural log of the input provided</returns>
/// <exception cref="InvalidArgumentError">Thrown when the input value is less than or equal to 0.5</exception>
let rec LogEGreaterThanZeroPointFive (input:float) (increment:float) (sum:float) =
    match input > 0.5 with 
    | true ->
        match increment with
        | 2000.0 -> sum
        | _  -> LogEGreaterThanZeroPointFive input (increment+1.0) (sum+(1.0/increment)*((input-1.0)/input)**increment)
    | false -> InvalidArgumentError "Invalid input passed to function, expected value above 0.5" |> raise
        
/// <summary>
/// Implements a Taylor-Series in order to approximate a value for LogE
/// </summary>
///
/// <remarks>
/// Reference :: https://www.efunda.com/math/taylor_series/logarithmic.cfm
/// </remarks>
///
/// <param name="input">A floating point value to take the logarithm base E of</param>
/// <param name="increment">
/// Current iteration of the taylor series, the more iterations performed, the more accurate
/// the approximation
/// </param>
/// <param name="sum">the cumulative sum of the Taylor Series</param>
///
/// <returns>The natural log of the input provided</returns>
/// <exception cref="InvalidArgumentError">Thrown when the input value is less than or equal to 0.5</exception>
let rec LogELessThanOrEqualToZeroPointFive (input:float) (increment:float) (sum:float) =
    match input > 0.5 || input <= 0.0 with
    | true ->
        InvalidArgumentError "Invalid input passed to function, expected value less than or equal to 0.5 "
        |> raise
    | false ->
        match increment with
        | 2000.0 -> sum
        | _ ->
            match (increment%2.0) with
            | 0.0 -> LogELessThanOrEqualToZeroPointFive input (increment+1.0) sum-(((input-1.0)**increment)/increment)
            | _ -> LogELessThanOrEqualToZeroPointFive input (increment+1.0) sum+(((input-1.0)**increment)/increment)

/// <summary>
/// Wrapper function for LogE, determines which version of the taylor series to call based on input as the
/// calculation used is different if performed on values less or equal to 0.5 
/// </summary>
///
/// <remarks>
/// Reference :: https://www.efunda.com/math/taylor_series/logarithmic.cfm
/// </remarks>
///
/// <param name="input">A floating point value to take the logarithm base E of</param>
///
/// <returns>The natural log of the input provided</returns>
let LogE input =
    match input <= 0.5 with
    | true -> LogELessThanOrEqualToZeroPointFive input 1.0 0.0
    | _ -> LogEGreaterThanZeroPointFive input 1.0 0.0

/// <summary>
/// Function to dynamically calculate any given base of any given number using the change of base rule
/// </summary>
///
/// <remarks>
/// Change of base rule :: log(_b)a = log(_d)a / log(_d)b
/// log(_d) in this formula is represented by logE however any log base would work
/// </remarks>
///
/// <param name="numerator">
/// A floating point value representing the top half of the change of base rule (a)
/// </param>
/// <param name="denominator">
/// A floating point value representing the bottom half of the change of base rule (b)
/// </param>
///
/// <returns>Value calculated as log(_b)a</returns>
let FormNewBaseRuleFraction numerator denominator =
    LogE numerator / LogE denominator

let Log2 input =
    FormNewBaseRuleFraction input 2.0
    
let Log10 input =
    FormNewBaseRuleFraction input 10.0
    
let LogX newBase input =
    FormNewBaseRuleFraction input newBase
    
let TerminalLog logFunction logVal=
    match logFunction with
    | nameof(LogE) -> LogE logVal |> Number
    | nameof(Log10) -> Log10 logVal |> Number
    | nameof(Log2) -> Log2 logVal |> Number
    | _ -> InvalidArgumentError "Invalid Arguments" |> raise
    
let RootToTerminals (terminals: terminal list) denominator =
    [Lpar; Lpar] @ terminals @ [Rpar; Exponent; Lpar; Number 1.0; Divide; Number denominator; Rpar; Rpar]
    
let FloorToNumber (numToFloor:float) =
    numToFloor |> int |> float
    
let CeilToNumber num =
    FloorToNumber num + 1.0
    
let RoundNum (num:float) =
    let trunkNum = num |> int
    let stringRep = string num
    let decimalVals = stringRep.Split[|'.'|]
    
    if decimalVals.Length = 2 then
        let numRep = decimalVals.[1] |> float
        if numRep >= 5.0 then
            trunkNum+1 |> float
        else
            trunkNum |> float
    else
        trunkNum |> float
    
let AbsVal (num:float) =
    +num

    