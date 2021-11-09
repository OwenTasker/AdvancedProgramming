/// <summary>
/// Module containing tests for the functions defined in Interpreter.Parser.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter.Tests</summary>
/// </namespacedoc>
module Interpreter.Tests.ParserTests

open NUnit.Framework
open Interpreter.Parser
open Interpreter.Util

/// <summary>List of test cases representing valid inputs to the parser.</summary>
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
        TestCaseData([Word "x";])
        TestCaseData([Lpar; Word "x"; Rpar])
        TestCaseData([Number 1.0; Plus; Word "x";])
        TestCaseData([Word "x"; Minus; Number 5.0;])
        TestCaseData([Number 1.0; Times; Number 5.0; Times; Lpar; Word "x"; Plus; Number 6.0; Rpar;])
        TestCaseData([Word "x"; Exponent; Number 5.0; Times; Lpar; Number 5.0; Plus; Number 6.0; Exponent; Word "x"; Rpar; Exponent; Number 2.0; Exponent; Lpar; Number 1.0; Rpar;])
        TestCaseData([Word "x"; Assign; Lpar; Number 10.0; Rpar])
        TestCaseData([Word "x"; Assign; Number 1.0; Plus; Number 5.0;])
        TestCaseData([Word "x"; Assign; Number 1.0; Minus; Number 5.0;])
        TestCaseData([Word "x"; Assign; Number 1.0; Times; Number 5.0;])
        TestCaseData([Word "x"; Assign; Number 1.0; Divide; Number 5.0;])
        TestCaseData([Word "x"; Assign; Number 1.0; Times; Number 5.0; Times; Lpar; Number 5.0; Plus; Number 6.0; Rpar;])
        TestCaseData([Word "x"; Assign; Number 2.0; Exponent; Number 3.0;])
        TestCaseData([Word "x"; Assign; Number 1.0; Exponent; Number 5.0; Times; Lpar; Number 5.0; Plus; Number 6.0; Exponent; Number 2.0; Rpar; Exponent; Number 2.0; Exponent; Lpar; Number 1.0; Rpar;])
        TestCaseData([Word "x"; Assign; Number 2.0; Plus; UnaryMinus; Number 2.0])
        TestCaseData([Word "x"; Assign; Number 2.0; Plus; UnaryPlus; Number 2.0])
        TestCaseData([Word "x"; Assign; Number 2.0; Plus; UnaryMinus; UnaryPlus; UnaryMinus; UnaryPlus; UnaryMinus; UnaryPlus; UnaryMinus; UnaryMinus; Number 2.0])
        TestCaseData([Word "x"; Assign; UnaryMinus; Number 2.0])
        TestCaseData([Word "x"; Assign; UnaryPlus; Number 2.0])
        TestCaseData([Function "floor"; Lpar; Number 2.0 ; Rpar])
        TestCaseData([Number 2.0; Plus; Function "floor"; Lpar; Number 2.0 ; Rpar])
        TestCaseData([Number 2.0; Plus; Function "floor"; Lpar; Number 2.0 ; Rpar; Divide; Number 2.0])
        TestCaseData([Function "floor"; Lpar; Number 2.0; Rpar; Plus; Function "floor"; Lpar; Number 2.0; Rpar])
        TestCaseData([Number 3.0; Plus; Function "floor"; Lpar; Number 2.0 ; Rpar; Plus; Number 2.0])
        TestCaseData([Number 3.0; Plus; Function "floor"; Lpar; Number 2.0 ; Rpar; Minus; Number 2.0])
        TestCaseData([Number 10.0; Plus; Function "floor"; Lpar; Number 2.0 ; Rpar; Divide; Number 2.0])
        TestCaseData([Number 2.0; Plus; Function "floor"; Lpar; Number 2.0 ; Rpar; Times; Number 2.0])
        TestCaseData([Number 2.0; Minus; Lpar; Function "floor"; Lpar; Number 2.0 ; Rpar; Rpar])
        TestCaseData([UnaryMinus; Function "floor"; Lpar; Number 2.0 ; Rpar;])
        TestCaseData([UnaryMinus; UnaryMinus; Function "floor"; Lpar; Number 2.0 ; Rpar;])
        TestCaseData([Function "floor"; Lpar; Number 2.0 ; Rpar;])
    ]

/// <summary>Test to ensure that statement correctly parses valid input.</summary>
[<TestCaseSource("ValidCases")>]
let GivenStatement_WhenPassedValidStatement_ReturnEmptyArray(terminals: terminal list) =
    let result = statement terminals
    Assert.That(result, Is.EqualTo([]))

/// <summary>Test to ensure that parse correctly parses valid input.</summary>
[<TestCaseSource("ValidCases")>]
let GivenParse_WhenPassedValidExpression_ReturnTrue(terminals: terminal list) =
     let result = parse terminals
     Assert.That(result, Is.EqualTo(true))

/// <summary>List of test cases representing invalid inputs to the parser.</summary>
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
        TestCaseData([Word "x"; Assign;])
        TestCaseData([Assign;])
        TestCaseData([Word "x"; Assign; Assign; UnaryPlus; Number 2.0])
        TestCaseData([Assign; UnaryPlus; Number 2.0])
        TestCaseData([Assign; Word "x"; Assign; Lpar; Number 10.0; Rpar])
        TestCaseData([Plus; Number 5.0;])
        TestCaseData([Number 1.0; Plus;])
        TestCaseData([Minus; Number 5.0;])
        TestCaseData([Number 1.0; Minus;])
        TestCaseData([Times; Number 5.0;])
        TestCaseData([Number 1.0; Times;])
        TestCaseData([Divide; Number 5.0;])
        TestCaseData([Number 1.0; Divide;])
        TestCaseData([Lpar; Number 1.0; Plus; Number 1.0])
        TestCaseData([Number 5.0; Lpar; Number 5.0; Plus; Number 6.0; Rpar;])
        TestCaseData([Lpar; Number 5.0; Plus; Number 6.0; Rpar; Number 5.0])
        TestCaseData([Number 2.0; Exponent])
        TestCaseData([Exponent; Number 5.0;])
        TestCaseData([Number 5.0; Number 5.0;])
        TestCaseData([Word "x"; Word "x";])
        TestCaseData([Number 5.0; Word "x";])
        TestCaseData([Word "x"; Number 5.0;])
        TestCaseData([Plus; Word "x";])
        TestCaseData([Word "x"; Plus;])
        TestCaseData([Minus; Word "x";])
        TestCaseData([Word "x"; Minus;])
        TestCaseData([Times; Word "x";])
        TestCaseData([Word "x"; Times;])
        TestCaseData([Divide; Word "x";])
        TestCaseData([Word "x"; Divide;])
        TestCaseData([Lpar; Number 1.0; Plus; Word "x"])
        TestCaseData([Word "x"; Lpar; Number 5.0; Plus; Number 6.0; Rpar;])
        TestCaseData([Lpar; Number 5.0; Plus; Number 6.0; Rpar; Word "x"])
        TestCaseData([Word "x"; Exponent])
        TestCaseData([Exponent; Word "x";])
    ]

/// <summary>Test to ensure that statement correctly throws an exception for invalid input.</summary>
[<TestCaseSource("InvalidCases")>]
let GivenStatement_WhenPassedInvalidExpression_ThenRaiseParseError(terminals: terminal list) =
    Assert.Throws<ParseError>(fun () -> statement terminals |> ignore) |> ignore

/// <summary>Test to ensure that parse correctly throws an exception for invalid input.</summary>
[<TestCaseSource("InvalidCases")>]
let GivenParse_WhenPassedInvalidExpression_ReturnFalse(terminals: terminal list) =
     let result = parse terminals
     Assert.That(result, Is.EqualTo(false))


