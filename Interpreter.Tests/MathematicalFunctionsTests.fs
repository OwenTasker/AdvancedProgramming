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

let LogEInputsOutputs = [
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

[<TestCaseSource("LogEInputsOutputs")>]
let givenLogE_ProvidedValidInput_ReturnCorrectApproximation input output =
    let logRes = LogE input
    let a = logRes > (output-0.000001)
    let b = logRes < (output+0.000001)
    Assert.True(a && b)
    
let Log2Inputs = [
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

[<TestCaseSource("Log2Inputs")>]
let givenLog2_ProvidedValidInput_ReturnCorrectApproximation input output =
    let logRes = Log2 input
    let a = logRes > (output-0.000001)
    let b = logRes < (output+0.000001)
    Assert.True(a && b)
    
let Log10Inputs = [
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

[<TestCaseSource("Log10Inputs")>]
let givenLog10_ProvidedValidInput_ReturnCorrectApproximation input output =
    let logRes = Log10 input
    let a = logRes > (output-0.000001)
    let b = logRes < (output+0.000001)
    Assert.True(a && b)
    
let LogXInputs = [
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

[<TestCaseSource("LogXInputs")>]
let givenLogX_ProvidedValidInput_ReturnCorrectApproximation logBase input output=
    let logRes = LogX logBase input
    let a = logRes > (output - 0.000001)
    let b = logRes < (output + 0.000001)
    Assert.True(a && b)
   
let InvalidLogInputs = [
    TestCaseData(0.0)
    TestCaseData(-0.5)
    TestCaseData(-1)
    TestCaseData(-10)
]

[<TestCaseSource("InvalidLogInputs")>]
let GivenLogE_ProvidedInvalidInput_ThrowInvalidArgumentError input =
    Assert.Throws<InvalidArgumentError>(fun () -> LogE input |> ignore) |> ignore
    
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