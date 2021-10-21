module Interpreter.Tests.ParserTests

open NUnit.Framework
open Interpreter.Parser
open Interpreter.Util

let ValidCases =
    [
        TestCaseData([Number 1.0;])
        TestCaseData([Lpar; Number 10.0; Rpar])
        TestCaseData([Number 1.0; Plus; Number 5.0;])
        TestCaseData([Number 1.0; Minus; Number 5.0;])
        TestCaseData([Number 1.0; Times; Number 5.0;])
        TestCaseData([Number 1.0; Divide; Number 5.0;])
        TestCaseData([Number 1.0; Times; Number 5.0; Times; Lpar; Number 5.0; Plus; Number 6.0; Rpar;])
        TestCaseData([Number 2.0; Exponent; Number 3.0;])
        TestCaseData([Number 1.0; Exponent; Number 5.0; Times; Lpar; Number 5.0; Plus; Number 6.0; Exponent; Number 2.0; Rpar; Exponent; Number 2.0; Exponent; Lpar; Number 1.0; Rpar;])
        TestCaseData([Number 2.0; Plus; UnaryMinus; Number 2.0])
        TestCaseData([Number 2.0; Plus; UnaryPlus; Number 2.0])
        TestCaseData([Number 2.0; Plus; UnaryMinus; UnaryPlus; UnaryMinus; UnaryPlus; UnaryMinus; UnaryPlus; UnaryMinus; UnaryMinus; Number 2.0])
        TestCaseData([UnaryMinus; Number 2.0])
        TestCaseData([UnaryPlus; Number 2.0])
    ]
    
[<TestCaseSource("ValidCases")>]
let GivenExpression_WhenPassedValidExpression_ReturnEmptyArray(terminals: terminal list) =
    let result = expression terminals
    Assert.That(result, Is.EqualTo([]))
    
[<TestCaseSource("ValidCases")>]
let GivenParse_WhenPassedValidExpression_ReturnTrue(terminals: terminal list) =
     let result = parse terminals
     Assert.That(result, Is.EqualTo(true))
    
let InvalidCases =
    [
        TestCaseData([Plus;])
        TestCaseData([Minus;])
        TestCaseData([Times;])
        TestCaseData([Divide;])
        TestCaseData([Exponent;])
        TestCaseData([Lpar])
        TestCaseData([Rpar])
        TestCaseData([UnaryPlus])
        TestCaseData([UnaryMinus])
        TestCaseData([Lpar; Rpar;])
        TestCaseData([Lpar; Plus; Rpar])
        TestCaseData([Plus; Number 5.0;])
        TestCaseData([Number 1.0; Plus;])
        TestCaseData([Minus; Number 5.0;])
        TestCaseData([Number 1.0; Minus;])
        TestCaseData([Times; Number 5.0;])
        TestCaseData([Number 1.0; Times;])
        TestCaseData([Divide; Number 5.0;])
        TestCaseData([Number 1.0; Divide;])
        TestCaseData([Lpar; Number 1.0; Plus; Number 1.0])
        TestCaseData([Lpar; Number 1.0; Plus; Number 1.0])
        TestCaseData([Number 5.0; Lpar; Number 5.0; Plus; Number 6.0; Rpar;])
        TestCaseData([Lpar; Number 5.0; Plus; Number 6.0; Rpar; Number 5.0])
        TestCaseData([Number 2.0; Exponent])
        TestCaseData([Exponent; Number 5.0;])
    ]
    
[<TestCaseSource("InvalidCases")>]
let GivenExpression_WhenPassedInvalidExpression_ThenRaiseParseError(terminals: terminal list) =
    Assert.Throws<ParseError>(fun () -> expression terminals |> ignore) |> ignore

[<TestCaseSource("InvalidCases")>]
let GivenParse_WhenPassedInvalidExpression_ReturnFalse(terminals: terminal list) =
     let result = parse terminals
     Assert.That(result, Is.EqualTo(false))
