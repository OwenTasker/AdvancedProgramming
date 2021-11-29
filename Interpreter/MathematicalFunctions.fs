﻿/// <summary>
/// Module containing all bespoke mathematical functions used, done to avoid using inbuilt libraries
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter.MathematicalFunctions</summary>
/// </namespacedoc>
module internal Interpreter.MathematicalFunctions

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
let rec private LogEGreaterThanZeroPointFive (input:float) (increment:float) (sum:float) =
        match input with
        | _ when input > 0.5 ->
            match increment with
            | 2000.0 -> sum
            | _  -> LogEGreaterThanZeroPointFive input (increment+1.0) (sum+(1.0/increment)*((input-1.0)/input)**increment)
        | _ -> InvalidArgumentError "Invalid input passed to function, expected value above 0.5" |> raise

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
let rec private LogELessThanOrEqualToZeroPointFive (input:float) (increment:float) (sum:float) =
    match input > 0.5 || input <= 0.0 with
    | true ->
        InvalidArgumentError "Invalid input passed to function, expected value less than or equal to 0.5 " |> raise
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
let private LogEFloat input =
    match input <= 0.5 with
    | true -> LogELessThanOrEqualToZeroPointFive input 1.0 0.0
    | _ -> LogEGreaterThanZeroPointFive input 1.0 0.0

let internal LogETerminal input =
    LogEFloat input |> Number
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
let private FormNewBaseRuleFraction numerator denominator =
    LogEFloat numerator / LogEFloat denominator

/// <summary>
/// Calculates the log_2 of the given input
/// </summary>
///
/// <param name="input">A floating point value to take the logarithm base 2 of</param>
///
/// <returns>The log base 2 of the input provided</returns>
let internal Log2 input =
    FormNewBaseRuleFraction input 2.0 |> Number

/// <summary>
/// Calculates the log_10 of the given input
/// </summary>
///
/// <param name="input">A floating point value to take the logarithm base 2 of</param>
///
/// <returns>The log base 2 of the input provided</returns>
let internal Log10 input =
    FormNewBaseRuleFraction input 10.0 |> Number

/// <summary>
/// Calculates the log_x of the given input
/// </summary>
///
/// <param name="newBase">the base you want to calculate</param>
/// <param name="input">A floating point value to take the logarithm base newBase of</param>
///
/// <returns>The terminal value of the log base newBase of the input provided</returns>
let internal LogX newBase input =
    if newBase = 0.0 && input = 0.0 then
        InvalidArgumentError "Expected Base to be greater than or equal to 0" |> raise
    elif newBase = 0.0 then
        0.0 |> Number
    else
        (FormNewBaseRuleFraction input newBase) |> Number

/// <summary>
/// Recreates a terminal list based on the provided square value
/// </summary>
///
/// <param name="terminals">All terminal values within function call</param>
/// <param name="denominator">Floating point value representing the root to calculate, 2 equals sqrt, 3 equals cbrt etc.</param>
///
/// <returns>
/// Returns a list of terminals equal to the value to calculate the root of to the power of 1/denominator
/// </returns>
let internal RootToTerminals (terminals: terminal list) denominator =
    if not terminals.IsEmpty && denominator <> [Number 0.0] then
        [Lpar; Lpar] @ terminals @ [Rpar; Exponent; Lpar; Number 1.0; Divide] @ denominator @ [Rpar; Rpar]
    else
        InvalidArgumentError "Ensure that input value is not empty and the root you are taking is not 0" |> raise


/// <summary>
/// Function to calculate the floor of a number, uses the fact that in F#, downcasting a float to int just truncates it
/// and doesnt take into account anything past the decimal point.
/// </summary>
///
/// <param name="numToFloor">Input to floor</param>
///
/// <returns>Returns the floored input</returns>
let internal FloorToTerminal (numToFloor:float) =
    let isNegative = numToFloor < 0.0
    let checkForInteger = numToFloor |> int |> float = numToFloor
    match checkForInteger with
    | true -> numToFloor |> Number
    | _ ->
        match isNegative with
        | false -> numToFloor |> int |> float |> Number
        | _ -> numToFloor - 1.0 |> int |> float |> Number

/// <summary>
/// Function to calculate the ceiling of a number, uses the fact that in F#, downcasting a float to int just truncates
/// it and doesnt take into account anything past the decimal point.
/// </summary>
///
/// <param name="numToCeil">Input to ceil</param>
///
/// <returns>Returns the ceiled input</returns>
let internal CeilToTerminal (numToCeil: float) =
    let isNegative = numToCeil < 0.0
    let checkForInteger  = numToCeil |> int |> float = numToCeil
    match checkForInteger with
    | true -> numToCeil |> Number
    | _ ->
        match isNegative with
        | false -> numToCeil + 1.0 |> int |> float |> Number
        | _ -> numToCeil |> int |> float |> Number

/// <summary>
/// Function to calculate the rounding of a number
/// </summary>
///
/// <param name="num">Input to round</param>
///
/// <returns>Returns the rounded input</returns>
let internal RoundNum (num:float) =
    let isInteger = (num |> int |> float = num)
    if isInteger then
        num |> Number
    else
        let trunkNum = num |> int
        let stringRep = string num
        let decimalVals = stringRep.Split[|'.'|]

        if decimalVals.Length = 2 then
            let numRep = decimalVals.[1] |> float
            if num > 0.0 then
                if numRep >= 5.0 then
                    trunkNum+1 |> float |> Number
                else
                    trunkNum |> float |> Number
            else
                if numRep >= 5.0 then
                    trunkNum-1 |> float |> Number
                else
                    trunkNum |> float |> Number
        else
            trunkNum |> float |> Number

/// <summary>
/// Function to calculate the absolute value of a number
/// </summary>
///
///<remarks>|num|</remarks>
///
/// <param name="num">Input to calculate the absolute value of</param>
///
/// <returns>Returns the absolute value of the input</returns>
let internal AbsVal (num:float) =
    match num < 0.0 with
    | true -> -num |> Number
    | _ -> num |> Number

/// <summary>
/// Function to calculate the greatest common divisor from a pair of integers
/// </summary>
///
/// <remarks>Input order is irrelevant</remarks>
///
/// <param name="num1">First value taken into account</param>
/// <param name="num2">Second value taken into account</param>
///
/// <returns>Return the greatest common denominator/highest common factor of the two provided values</returns>
let rec private getGCD (num1:float) (num2:float) =
    if num2 = 0.0 then
        AbsVal num1
    else
        getGCD num2 (num1%num2)

/// <summary>
/// Wrapper function for greatest common divisor from a pair of integers, contains logic in which to call getGCD with
/// </summary>
///
/// <remarks>Input order is irrelevant</remarks>
///
/// <param name="num1">First value taken into account</param>
/// <param name="num2">Second value taken into account</param>
///
/// <returns>Returns the Greatest Common Denominator/Highest Common Factor of the two provided values</returns>
let internal getGCDWrapper num1 num2 =
    let areBothInputsIntegers = (num1 |> int |> float = num1) && (num2 |> int |> float = num2)
    let areBothIntegersNotZero = (num1 <> 0.0) && (num2 <> 0.0)

    if not areBothIntegersNotZero then
        0.0 |> Number
    elif areBothInputsIntegers then
        getGCD num1 num2
    else
        InvalidArgumentError "Ensure both arguments are whole numbers" |> raise


/// <summary>
/// Function to calculate the modulo of a number by another number
/// </summary>
///
/// <remarks>num1 mod num2</remarks>
///
/// <param name="num1">Original Number</param>
/// <param name="num2">Number to divide num1 by</param>
///
/// <returns>Returns the absolute value of the input</returns>
let internal moduloCalc (num1:float) (num2:float) =
    let isNum1Negative =  num1 < 0.0
    let isNum2Negative =  num2 < 0.0
    let isNum2Zero = num2 = 0.0

    let areEitherNumNegative = isNum1Negative && isNum2Negative
    match not areEitherNumNegative && not isNum2Zero with
    | true ->
        num1%num2 |> Number
    | false ->
        InvalidArgumentError "Only pass positive values to modulo function and non-zero values as second value" |> raise

/// <summary>
/// Function to return a random whole number between specified lower and upper bound
/// </summary>
///
/// <param name="num1">Lower bound</param>
/// <param name="num2">Upper bound</param>
///
/// <returns>Returns a random Number terminal between a lower and upper bound </returns>
let internal pseudoRandom (num1:float) (num2:float) =
    let isNum1Whole = (num1 |> int |> float) = num1
    let isNum2Whole = (num2 |> int |> float) = num2
    if (isNum1Whole && isNum2Whole) then
        System.Random().Next(num1 |> int, num2 |> int) |> float |> Number
    else
        InvalidArgumentError "Ensure both values passed to rand are whole numbers" |> raise