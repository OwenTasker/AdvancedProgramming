/// <summary>
/// Module containing tests for the functions defined in Interpreter.MathematicalFunctions.fs.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter.Tests</summary>
/// </namespacedoc>
module Interpreter.Tests.MathematicalFunctionsTests

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
    let logRes = LogE' input
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
    