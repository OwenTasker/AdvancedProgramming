module Interpreter.Tests.DifferentiateTests

open NUnit.Framework
open Interpreter.Differentiate
open Interpreter.Util

// Test that perform operation throws errors
// Test that auto differentiate performs correct expected differentiation

// terminal list -> terminal list -> Dual list -> terminal list

let AutoDifferentiateCases =
    [
        TestCaseData([Number 2.0], [Number 0.0])
        TestCaseData([Word "x"], [Number 1.0])
        TestCaseData([Word "x"; Plus; Word "x"], [Number 2.0])
        TestCaseData([Word "x"; Times; Word "x"], [Word "x"; Plus; Word "x"])
        TestCaseData([Word "x"; Exponent; Number 2.0], [Number 2.0; Times; Word "x"; Exponent; Number 1.0])
        TestCaseData([Number 2.0; Exponent; Word "x"], [Function "ln"; Lpar; Number 2.0; Rpar; Times; Number 2.0; Exponent; Word "x"])
    ]

// Test that auto differentiate throws errors
// Test that differentiate performs correct expected differentiation

[<TestCaseSource("AutoDifferentiateCases")>]
let GivenDifferentiate_WhenPassedValidExpression_ReturnFirstDerivative(expression : terminal list, expected : terminal list) =
    let result = differentiate expression
    Assert.That(result, Is.EqualTo(expected))
