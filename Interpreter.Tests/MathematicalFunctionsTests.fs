/// <summary>
/// Module containing tests for the functions defined in Interpreter.MathematicalFunctions.fs.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter.Tests</summary>
/// </namespacedoc>
module Interpreter.Tests.MathematicalFunctionsTests

open Interpreter.Util
open NUnit.Framework
open Interpreter.MathematicalFunctions

//Valid Log Inputs and Outputs
let logEGreaterThanZeroPointFiveValidInputsOutputs =
    [
    TestCaseData(0.6, -0.510825)
    TestCaseData(0.75, -0.287682)
    TestCaseData(0.9, -0.105360)
    TestCaseData(1.0, 0.0)
    TestCaseData(1.5, 0.405465)
    TestCaseData(2.0, 0.693147)
    TestCaseData(2.718281, 1.0)
    TestCaseData(3.0, 1.098612)
    TestCaseData(5.0, 1.609437)
    TestCaseData(10.0, 2.302585)
    TestCaseData(100.0, 4.605170)
    ]

let LogELessThanOrEqualToZeroPointFiveValidInputsOutputs =
    [
        TestCaseData(0.5, -0.693147)
        TestCaseData(0.25, -1.386294)
        TestCaseData(0.01, -4.605170)
    ]

let LogEValidInputsOutputs =
    [
    TestCaseData(0.01, -4.605170)
    TestCaseData(0.25, -1.386294)
    TestCaseData(0.5, -0.693147)
    TestCaseData(0.6, -0.510825)
    TestCaseData(0.75, -0.287682)
    TestCaseData(0.9, -0.105360)
    TestCaseData(1.0, 0.0)
    TestCaseData(1.5, 0.405465)
    TestCaseData(2.0, 0.693147)
    TestCaseData(2.718281, 1.0)
    TestCaseData(3.0, 1.098612)
    TestCaseData(5.0, 1.609437)
    TestCaseData(10.0, 2.302585)
    TestCaseData(100.0, 4.605170)
    ]

let Log2ValidInputOutputs =
    [
    TestCaseData(0.25, -2.0)
    TestCaseData(0.5, -1)
    TestCaseData(0.6, -0.736965)
    TestCaseData(0.75, -0.415037)
    TestCaseData(0.9, -0.152003)
    TestCaseData(1.0, 0.0)
    TestCaseData(1.5, 0.584962)
    TestCaseData(2.0, 1.0)
    TestCaseData(2.718281, 1.442694)
    TestCaseData(3.0, 1.584962)
    TestCaseData(5.0, 2.321928)
    TestCaseData(10.0, 3.321928)
    TestCaseData(100.0, 6.643856)
    ]

let Log10ValidInputOutputs =
    [
    TestCaseData(0.25, -0.602059)
    TestCaseData(0.5, -0.301029)
    TestCaseData(0.6, -0.221848)
    TestCaseData(0.75, -0.124938)
    TestCaseData(0.9, -0.045757)
    TestCaseData(1.0, 0.0)
    TestCaseData(1.5, 0.176091)
    TestCaseData(2.0, 0.301029)
    TestCaseData(2.718281, 0.434294)
    TestCaseData(3.0, 0.477121)
    TestCaseData(5.0, 0.698970)
    TestCaseData(10.0, 1.0)
    TestCaseData(100.0, 2.0)
    ]

let LogXValidInputOutputs =
    [
    TestCaseData(0.0, 1.0, 0.0)
    TestCaseData(0.0, 5.0, 0.0)
    
    TestCaseData(3.0, 0.25, -1.261859)
    TestCaseData(3.0, 0.5, -0.630929)
    TestCaseData(3.0, 0.6, -0.464973)
    TestCaseData(3.0, 0.75, -0.261859)
    TestCaseData(3.0, 0.9, -0.095903)
    TestCaseData(3.0, 1.0, 0.0)
    TestCaseData(3.0, 1.5, 0.369070)
    TestCaseData(3.0, 2.0, 0.630929)
    TestCaseData(3.0, 2.718281, 0.910239)
    TestCaseData(3.0, 3.0, 1.0)
    TestCaseData(3.0, 5.0, 1.464973)
    TestCaseData(3.0, 10.0, 2.095903)
    TestCaseData(3.0, 100.0, 4.191806)
    
    TestCaseData(5, 0.25, -0.861353)
    TestCaseData(5, 0.5, -0.430676)
    TestCaseData(5, 0.6, -0.317393)
    TestCaseData(5, 0.75, -0.178746)
    TestCaseData(5, 0.9, -0.065464)
    TestCaseData(5, 1.0, 0.0)
    TestCaseData(5, 1.5, 0.251929)
    TestCaseData(5, 2.0, 0.430676)
    TestCaseData(5, 2.718281, 0.621334)
    TestCaseData(5, 3.0, 0.682606)
    TestCaseData(5, 5.0, 1.0)
    TestCaseData(5, 10.0, 1.430676)
    TestCaseData(5, 100.0, 2.861353)
    ]

//Invalid Log Inputs
let GenericInvalidLogInputs =
    [
        TestCaseData(0.0)
        TestCaseData(-0.1)
        TestCaseData(-1.0)
        TestCaseData(-5.0)
    ]

let logEGreaterThanZeroPointFiveInvalidInputs =
    [
    TestCaseData(0.5)
    TestCaseData(0.25)
    ]

let LogELessThanOrEqualToZeroPointFiveInvalidInputs =
    [
        TestCaseData(0.51)
        TestCaseData(1.0)
        TestCaseData(5.0)
    ]
    
let LogXInvalidInputs =
    [   
    TestCaseData(3.0, 0.0)
    TestCaseData(3.0, -0.1)
    TestCaseData(3.0, -1.0)
    TestCaseData(3.0, -5.0)
    
    TestCaseData(5.0, 0.0)
    TestCaseData(5.0, -0.1)
    TestCaseData(5.0, -1.0)
    TestCaseData(5.0, -5.0)
    
    TestCaseData(7.0, 0.0)
    TestCaseData(7.0, -0.1)
    TestCaseData(7.0, -1.0)
    TestCaseData(7.0, -5.0)
        
    TestCaseData(0.0, 0.0)
    TestCaseData(-1.0, 0.0)
    TestCaseData(-5.0, -0.1)
    TestCaseData(-1.0, -5.0)
    ]
    

//Valid Log Test Cases
[<TestCaseSource(nameof logEGreaterThanZeroPointFiveValidInputsOutputs)>]
let givenLogEGreaterThanZeroPointFive_ProvidedValidInput_ReturnCorrectValue input output =
    let logRes = LogEGreaterThanZeroPointFive input 1.0 0.0
    let a = logRes > (output-0.000001)
    let b = logRes < (output+0.000001)
    Assert.True(a && b)

[<TestCaseSource(nameof LogELessThanOrEqualToZeroPointFiveValidInputsOutputs)>]
let givenLogELessThanOrEqualToZeroPointFive_ProvidedValidInput_ReturnCorrectValue input output =
    let logRes = LogELessThanOrEqualToZeroPointFive input 1.0 0.0
    let a = logRes > (output-0.000001)
    let b = logRes < (output+0.000001)
    Assert.True(a && b)

[<TestCaseSource(nameof LogEValidInputsOutputs)>]
let givenLogE_ProvidedValidInput_ReturnCorrectApproximation input output =
    let logRes = LogE input
    let a = logRes > (output-0.000001)
    let b = logRes < (output+0.000001)
    Assert.True(a && b)
    
[<TestCaseSource(nameof Log2ValidInputOutputs)>]
let givenLog2_ProvidedValidInput_ReturnCorrectApproximation input output =
    let logRes = Log2 input
    let a = logRes > (output-0.000001)
    let b = logRes < (output+0.000001)
    Assert.True(a && b)
    
[<TestCaseSource(nameof Log10ValidInputOutputs)>]
let givenLog10_ProvidedValidInput_ReturnCorrectApproximation input output =
    let logRes = Log10 input
    let a = logRes > (output-0.000001)
    let b = logRes < (output+0.000001)
    Assert.True(a && b)
    
[<TestCaseSource(nameof LogXValidInputOutputs)>]
let givenLogX_ProvidedValidInput_ReturnCorrectApproximation logBase input output =
    let logRes = LogX logBase input
    let a = logRes > (output - 0.000001 |> Number)
    let b = logRes < (output + 0.000001 |> Number)
    Assert.True(a && b)

//Invalid Log Test Cases   
[<TestCaseSource(nameof GenericInvalidLogInputs)>]
[<TestCaseSource(nameof logEGreaterThanZeroPointFiveInvalidInputs)>]
let givenLogEGreaterThanZeroPointFive_ProvidedInvalidInput_ThrowInvalidArgumentError input =
    Assert.Throws<InvalidArgumentError>(fun () -> LogEGreaterThanZeroPointFive input 1.0 0.0 |> ignore) |> ignore

[<TestCaseSource(nameof GenericInvalidLogInputs)>]
[<TestCaseSource(nameof LogELessThanOrEqualToZeroPointFiveInvalidInputs)>]
let givenLogELessThanOrEqualToZeroPointFive_ProvidedInvalidInput_ThrowInvalidArgumentError input =
    Assert.Throws<InvalidArgumentError>(fun () -> LogELessThanOrEqualToZeroPointFive input 1.0 0.0 |> ignore) |> ignore

[<TestCaseSource(nameof GenericInvalidLogInputs)>]
let givenLog10_ProvidedInvalidInputs_ThrowInvalidArgumentError input =
    Assert.Throws<InvalidArgumentError>(fun () -> Log10 input |> ignore) |> ignore
    
[<TestCaseSource(nameof GenericInvalidLogInputs)>]
let givenLog2_ProvidedInvalidInputs_ThrowInvalidArgumentError input =
    Assert.Throws<InvalidArgumentError>(fun () -> Log2 input |> ignore) |> ignore
   
[<TestCaseSource(nameof GenericInvalidLogInputs)>]
let givenLogE_ProvidedInvalidInputs_ThrowInvalidArgumentError input =
    Assert.Throws<InvalidArgumentError>(fun () -> LogE input |> ignore) |> ignore

[<TestCaseSource(nameof LogXInvalidInputs)>]
let givenLogX_ProvidedInvalidInputs_ThrowInvalidArgumentError newBase input =
    Assert.Throws<InvalidArgumentError>(fun () -> (LogX newBase input) |> ignore) |> ignore  
   
   
   
   
   
   
   

   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    
let ValidRootToTerminalsInputs = [
        TestCaseData(
            [Number 10.0],
            2,
            [Lpar; Lpar; Number 10.0; Rpar; Exponent; Lpar; Number 1.0; Divide; Number 2.0; Rpar; Rpar])
        TestCaseData(
            [Number 10.0;Divide;Number 3.0],
            2,
            [Lpar; Lpar; Number 10.0; Divide; Number 3.0; Rpar; Exponent; Lpar; Number 1.0; Divide; Number 2.0; Rpar; Rpar])
    ]

[<TestCaseSource(nameof(ValidRootToTerminalsInputs))>]
let GivenRootToTerminals_ProvidedValidInput_ReturnCorrectOutput inputVals whichRoot output =
    Assert.That(RootToTerminals inputVals whichRoot, Is.EqualTo(output))