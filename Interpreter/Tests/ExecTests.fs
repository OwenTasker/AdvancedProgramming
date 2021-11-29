﻿/// <summary>
/// Module containing tests for the functions defined in Interpreter.Exec.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter.Tests</summary>
/// </namespacedoc>
module Interpreter.Tests.ExecTests

open System.Collections.Generic
open NUnit.Framework
open Interpreter.Exec
open Interpreter.Util

/// <summary>Test cases for valid input to reduce, reduceRecursive and exec without variables.</summary>
let ValidNoVariablesReduceCases =
    [
        //SIMPLE ADDITION CASES
        TestCaseData([Number 1.0; Plus; Number 1.0], Number 2.0)
        TestCaseData([Number 1.0; Plus; Number 0.0], Number 1.0)
        TestCaseData([Number 0.0; Plus; Number 67.0], Number 67.0)
        TestCaseData([Number 0.0; Plus; Number 0.0], Number 0.0)
        TestCaseData([Number 5.0; Plus; Number -5.0], Number 0.0)
        TestCaseData([Number -5.0; Plus; Number -5.0], Number -10.0)
        TestCaseData([Number -5.0; Plus; Number 5.0], Number 0.0)
        TestCaseData([Number 10.0; Plus; Number -100.0], Number -90.0)
        //SIMPLE SUBTRACTION CASES
        TestCaseData([Number 1.0; Minus; Number 1.0], Number 0.0)
        TestCaseData([Number 1.0; Minus; Number 0.0], Number 1.0)
        TestCaseData([Number 0.0; Minus; Number 67.0], Number -67.0)
        TestCaseData([Number 0.0; Minus; Number 0.0], Number 0.0)
        TestCaseData([Number 5.0; Minus; Number -5.0], Number 10.0)
        TestCaseData([Number -5.0; Minus; Number -5.0], Number 0.0)
        TestCaseData([Number -5.0; Minus; Number 5.0], Number -10.0)
        TestCaseData([Number 10.0; Minus; Number -100.0], Number 110.0)
        //SIMPLE MULTIPLY CASES
        TestCaseData([Number 1.0; Times; Number 8.0], Number 8.0)
        TestCaseData([Number 8.0; Times; Number 1.0], Number 8.0)
        TestCaseData([Number 0.0; Times; Number 11.0], Number 0.0)
        TestCaseData([Number 10.0; Times; Number 0.0], Number 0.0)
        TestCaseData([Number 0.0; Times; Number 0.0], Number 0.0)
        TestCaseData([Number -10.0; Times; Number -10.0], Number 100.0)
        TestCaseData([Number 10.0; Times; Number 10.0], Number 100.0)
        TestCaseData([Number -10.0; Times; Number 10.0], Number -100.0)
        TestCaseData([Number 10.0; Times; Number -10.0], Number -100.0)
        //SIMPLE DIVIDE CASES
        TestCaseData([Number 1.0; Divide; Number 8.0], Number 0.125)
        TestCaseData([Number 8.0; Divide; Number 1.0], Number 8.0)
        TestCaseData([Number 0.0; Divide; Number 11.0], Number 0.0)
        TestCaseData([Number -10.0; Divide; Number -10.0], Number 1.0)
        TestCaseData([Number 10.0; Divide; Number 10.0], Number 1.0)
        TestCaseData([Number -10.0; Divide; Number 10.0], Number -1.0)
        TestCaseData([Number 10.0; Divide; Number -10.0], Number -1.0)
        //SIMPLE EXPONENT CASES
        TestCaseData([Number 1.0; Exponent; Number 8.0], Number 1.0)
        TestCaseData([Number 8.0; Exponent; Number 1.0], Number 8.0)
        TestCaseData([Number 0.0; Exponent; Number 11.0], Number 0.0)
        TestCaseData([Number 11.0; Exponent; Number 0.0], Number 1.0)
        TestCaseData([Number 0.0; Exponent; Number 0.0], Number 1.0)
        TestCaseData([Number -10.0; Exponent; Number -10.0], Number (-10.0 ** -10.00))
        TestCaseData([Number 10.0; Exponent; Number 2.0], Number 100.0)
        TestCaseData([Number -10.0; Exponent; Number 2.0], Number 100.0)
        TestCaseData([Number 10.0; Exponent; Number 3.0], Number 1000.0)
        TestCaseData([Number -10.0; Exponent; Number 3.0], Number -1000.0)
        TestCaseData([Number 10.0; Exponent; Number -3.0], Number 0.001)
        TestCaseData([Number -10.0; Exponent; Number -3.0], Number -0.001)
        //SIMPLE UNARY CASES
        TestCaseData([UnaryMinus; Number 1.0], Number -1.0)
        TestCaseData([UnaryMinus; Number -1.0], Number 1.0)
        TestCaseData([UnaryPlus; Number 1.0], Number 1.0)
        TestCaseData([UnaryPlus; Number -1.0], Number -1.0)
        //CHAINED UNARY CASES
        TestCaseData([UnaryMinus; UnaryMinus; Number 1.0], Number 1.0)
        TestCaseData([UnaryMinus; UnaryMinus; Number -1.0], Number -1.0)
        TestCaseData([UnaryPlus; UnaryPlus; Number 1.0], Number 1.0)
        TestCaseData([UnaryPlus; UnaryPlus; Number -1.0], Number -1.0)
        //ORDER OF OPERATIONS CASES
        TestCaseData([Number 1.0; Plus; Number 8.0; Minus; Number 6.0], Number 3.0)
        TestCaseData([Number 1.0; Minus; Number 8.0; Plus; Number 6.0], Number -1.0)
        TestCaseData([Number 64.0; Times; Number 8.0; Divide; Number 4.0], Number 128.0)
        TestCaseData([Number 64.0; Divide; Number 4.0; Times; Number 8.0], Number 128.0)
        TestCaseData([Number 7.0; Plus; Number 8.0; Times; Number 6.0], Number 55.0)
        TestCaseData([Number 9.0; Times; Number 8.0; Plus; Number 6.0], Number 78.0)
        TestCaseData([Number 6.0; Times; Number 8.0; Minus; Number 4.0], Number 44.0)
        TestCaseData([Number 6.0; Minus; Number 4.0; Times; Number 8.0], Number -26.0)
        TestCaseData([Number 1.0; Plus; Number 8.0; Divide; Number 4.0], Number 3.0)
        TestCaseData([Number 1.0; Divide; Number 8.0; Plus; Number 6.0], Number 6.125)
        TestCaseData([Number 64.0; Divide; Number 8.0; Minus; Number 4.0], Number 4.0)
        TestCaseData([Number 64.0; Minus; Number 4.0; Divide; Number 8.0], Number 63.5)
        TestCaseData([Number 1.0; Plus; Number 8.0; Exponent; Number 2.0], Number 65.0)
        TestCaseData([Number 1.0; Exponent; Number 8.0; Plus; Number 6.0], Number 7.0)
        TestCaseData([Number 2.0; Minus; Number 8.0; Exponent; Number 2.0], Number -62.0)
        TestCaseData([Number 16.0; Exponent; Number 2.0; Minus; Number 8.0], Number 248.0)
        TestCaseData([Number 7.0; Times; Number 5.0; Exponent; Number 3.0], Number 875.0)
        TestCaseData([Number 9.0; Exponent; Number 2.0; Times; Number 6.0], Number 486.0)
        TestCaseData([Number 64.0; Divide; Number 8.0; Exponent; Number 2.0], Number 1.0)
        TestCaseData([Number 6.0; Exponent; Number 3.0; Divide; Number 2.0], Number 108.0)
        TestCaseData([Lpar; Number 1.0; Plus; Number 8.0; Rpar; Minus; Number 6.0], Number 3.0)
        TestCaseData([Lpar; Number 1.0; Minus; Number 8.0; Rpar; Plus; Number 6.0], Number -1.0)
        TestCaseData([Lpar; Number 64.0; Times; Number 8.0; Rpar; Divide; Number 4.0], Number 128.0)
        TestCaseData([Lpar; Number 64.0; Divide; Number 4.0; Rpar; Times; Number 8.0], Number 128.0)
        TestCaseData([Lpar; Number 7.0; Plus; Number 8.0; Rpar; Times; Number 6.0], Number 90.0)
        TestCaseData([Lpar; Number 9.0; Times; Number 8.0; Rpar; Plus; Number 6.0], Number 78.0)
        TestCaseData([Lpar; Number 6.0; Times; Number 8.0; Rpar; Minus; Number 4.0], Number 44.0)
        TestCaseData([Lpar; Number 6.0; Minus; Number 4.0; Rpar; Times; Number 8.0], Number 16.0)
        TestCaseData([Lpar; Number 1.0; Plus; Number 8.0; Rpar; Divide; Number 4.0], Number 2.25)
        TestCaseData([Lpar; Number 1.0; Divide; Number 8.0; Rpar; Plus; Number 6.0], Number 6.125)
        TestCaseData([Lpar; Number 64.0; Divide; Number 8.0; Rpar; Minus; Number 4.0], Number 4.0)
        TestCaseData([Lpar; Number 64.0; Minus; Number 4.0; Rpar; Divide; Number 8.0], Number 7.5)
        TestCaseData([Lpar; Number 1.0; Plus; Number 8.0; Rpar; Exponent; Number 2.0], Number 81.0)
        TestCaseData([Lpar; Number 1.0; Exponent; Number 8.0; Rpar; Plus; Number 6.0], Number 7.0)
        TestCaseData([Lpar; Number 2.0; Minus; Number 8.0; Rpar; Exponent; Number 2.0], Number 36.0)
        TestCaseData([Lpar; Number 16.0; Exponent; Number 2.0; Rpar; Minus; Number 8.0], Number 248.0)
        TestCaseData([Lpar; Number 7.0; Times; Number 5.0; Rpar; Exponent; Number 3.0], Number 42875.0)
        TestCaseData([Lpar; Number 9.0; Exponent; Number 2.0; Rpar; Times; Number 6.0], Number 486.0)
        TestCaseData([Lpar; Number 64.0; Divide; Number 8.0; Rpar; Exponent; Number 2.0], Number 64.0)
        TestCaseData([Lpar; Number 6.0; Exponent; Number 3.0; Rpar; Divide; Number 2.0], Number 108.0)
        TestCaseData([UnaryMinus; Lpar; UnaryPlus; Number 6.0; Exponent; UnaryMinus; Number 3.0; Rpar; Divide; Number 2.0], Number ((-1.0/6.0**3.0)/2.0))
        TestCaseData([UnaryMinus; Lpar; UnaryPlus; Number 6.0; Divide; UnaryMinus; Number 3.0; Rpar; Exponent; Number 2.0], Number 4.0)
        TestCaseData([UnaryMinus; Number 2.0; Exponent; Number 3.0], Number -8.0)
    ]

/// <summary>Test to ensure that exec returns the correct output with valid input without variables.</summary>
[<TestCaseSource("ValidNoVariablesReduceCases")>]
let GivenExec_WhenPassedSimpleExpressionWithoutVariables_ReturnCorrectAnswer(tokens: terminal list, expected: terminal) =
    let result = exec Map.empty tokens
    Assert.That(result, Is.EqualTo([expected], Map.empty |> Map.toSeq |> dict))

/// <summary>Test cases for invalid input to reduce and reduceRecursive.</summary>
let InvalidReduceCases =
    [
        TestCaseData([Comma;])
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
        TestCaseData([Assign;])
        TestCaseData([Word "x"; Assign])
    ]

/// <summary>Test to ensure that exec returns the correct output with invalid input.</summary>
[<TestCaseSource("InvalidReduceCases")>]
let GivenExed_WhenPassedInvalidTokens_RaiseExecError(tokens: terminal list) =
    Assert.Throws<ExecError>(fun () -> exec Map.empty tokens  |> ignore) |> ignore


/// <summary>Test cases for valid input to getPrecedence.</summary>
let ValidGetPrecedenceData =
    [
        TestCaseData(UnaryMinus, 4)
        TestCaseData(UnaryPlus, 4)
        TestCaseData(Exponent, 3)
        TestCaseData(Times, 2)
        TestCaseData(Divide, 2)
        TestCaseData(Plus, 1)
        TestCaseData(Minus, 1)
    ]

/// <summary>Test cases for valid input to getAssociativity.</summary>
let ValidGetAssociativityData =
    [
        TestCaseData(UnaryMinus, "r")
        TestCaseData(UnaryPlus, "r")
        TestCaseData(Exponent, "r")
        TestCaseData(Times, "l")
        TestCaseData(Divide, "l")
        TestCaseData(Plus, "l")
        TestCaseData(Minus, "l")
    ]

/// <summary>Test to ensure that getPrecedence performs correctly under valid input.</summary>
[<TestCaseSource("ValidGetPrecedenceData")>]
let GivenGetPrecedence_WhenPassedOperatorWithPrecedence_ReturnCorrectPrecedence(operator: terminal, precedence: int) =
    let result = getPrecedence operator
    Assert.That(result, Is.EqualTo(precedence))

/// <summary>Test to ensure that getAssociativity performs correctly under valid input.</summary>
[<TestCaseSource("ValidGetAssociativityData")>]
let GivenGetAssociativity_WhenPassedOperatorWithAssociativity_ReturnCorrectAssociativity(operator: terminal, associativity: string) =
    let result = getAssociativity operator
    Assert.That(result, Is.EqualTo(associativity))

/// <summary>Test cases for invalid input to getPrecedence and getAssociativity.</summary>
let InvalidGetPrecedenceAssociativityCases =
    [
        TestCaseData(Lpar)
        TestCaseData(Rpar)
        TestCaseData(Number 1.0)
    ]

/// <summary>Test to ensure that getPrecedence correctly throws exception with invalid input.</summary>
[<TestCaseSource("InvalidGetPrecedenceAssociativityCases")>]
let GivenGetPrecedence_WhenPassedOperatorNotInMap_RaiseKeyNotFoundException(operator: terminal) =
    Assert.Throws<KeyNotFoundException>(fun () -> getPrecedence operator |> ignore) |> ignore

/// <summary>Test to ensure that getAssociativity correctly throws exception with invalid input.</summary>
[<TestCaseSource("InvalidGetPrecedenceAssociativityCases")>]
let GivenGetAssociativity_WhenPassedOperatorNotInMap_RaiseKeyNotFoundException(operator: terminal) =
    Assert.Throws<KeyNotFoundException>(fun () -> getAssociativity operator |> ignore) |> ignore

/// <summary>Example environment for testing.</summary>
let env = Map [("x", [Number 2.0])
               ("y", [Number 1.0; Plus; Word "x"])
               ("z", [Word "a"])
               ("q", [Word "x"; Plus; Word "z"])]

/// <summary>Test cases for valid inputs to closed.</summary>
let ClosedCases =
    [
        TestCaseData(([] : terminal list), true)
        TestCaseData([Number 1.0], true)
        TestCaseData([Word "x"], true)
        TestCaseData([Word "x"; Plus; Number 1.0], true)
        TestCaseData([Word "x"; Plus; Word "y"], true)
        TestCaseData([Word "z"], false)
        TestCaseData([Word "q"], false)
        TestCaseData([Word "x"; Plus; Word "z"], false)
    ]

/// <summary>Test to ensure that closed correctly recognises closed and free expressions.</summary>
[<TestCaseSource("ClosedCases")>]
let GivenClosed_WhenPassedExpression_ReturnCorrectBoolean(terminals: terminal list, expected: bool) =
    let result = closed env terminals |> fst
    Assert.That(result, Is.EqualTo(expected))

let TestTests =
    [
        TestCaseData(true, true)
    ]

[<TestCaseSource("TestTests")>]
let TestTestTests(input: bool, output: bool) =
    Assert.That(input, Is.EqualTo(output))

/// <summary>Test cases for valid assign inputs to exec.</summary>
let ValidExecAssignCases =
    [
        TestCaseData([Word "x"; Assign; Number 2.0], ("x", [Number 2.0]))
        TestCaseData([Word "x"; Assign; Word "x";], ("x", [Number 2.0]))
        TestCaseData([Word "x"; Assign; Word "x"; Plus; Word "x"], ("x", [Number 4.0]))
        TestCaseData([Word "y"; Assign; Word "z"], ("y", [Word "z"]))
        TestCaseData([Word "b"; Assign; Word "x"; Times; Word "y"], ("b", [Number 6.0]))
        TestCaseData([Word "x"; Assign; Word "x"; Plus; Number 1.0], ("x", [Number 3.0]))
    ]

/// <summary>Test to ensure that exec correctly updates environment when passed valid assign.</summary>
[<TestCaseSource("ValidExecAssignCases")>]
let GivenExec_WhenPassedValidAssign_ThenAddToEnvAndReturn(terminals: terminal list, entry: string*terminal list) =
    let result = exec env terminals
    Assert.That(result, Is.EqualTo((terminals, (env.Add entry))))

/// <summary>Test cases for valid user function calls to exec.</summary>
let UserFunctionCases =
    [
        TestCaseData([Function "y"; Lpar; Rpar;], Map [("y", [Number 2.0])], [Number 2.0])
        TestCaseData([Function "y"; Lpar; Rpar;], Map [("y", [Word "x"]); ("x", [Number 2.0])], [Number 2.0])
        TestCaseData([Function "y"; Lpar; Word "x"; Assign; Number 2.0; Rpar;], Map [("y", [Word "x"]); ("x", [Number 2.0])], [Number 2.0])
        TestCaseData([Function "y"; Lpar; Word "x"; Assign; Number 4.0; Rpar;], Map [("y", [Word "x"])], [Number 4.0])
        TestCaseData([Function "y"; Lpar; Word "x"; Assign; Number 4.0; Rpar;], Map [("y", [Word "x"; Exponent; Number 2.0])], [Number 16.0])
        TestCaseData([Function "y"; Lpar; Word "x"; Assign; Number 4.0; Comma; Word "z"; Assign; Number 3.0; Rpar;], Map [("y", [Word "x"; Exponent; Number 2.0; Plus; Word "z"])], [Number 19.0])
    ]

/// <summary>Test to ensure that exec returns correctly result with valid user function call.</summary>
[<TestCaseSource("UserFunctionCases")>]
let GivenExec_WhenPassedValidUserFunctionCall_ReturnCorrectResult(terminals: terminal list, env: Map<string, terminal list>, expected: terminal list) =
    let result = exec env terminals
    Assert.That(result, Is.EqualTo((expected, env |> Map.toSeq |> dict)))

/// <summary>Test cases for invalid input to exec with user functions.</summary>
let UserFunctionErrorCases =
    [
        TestCaseData([Function "y"; Lpar;], Map [("y", [Number 2.0])])
        TestCaseData([Function "y"; Lpar; Comma; Rpar;], Map [("y", [Word "x"])])
        TestCaseData([Function "y"; Lpar; Rpar;], Map [("y", [Word "x"]); ("x", [Word "z"])])
        TestCaseData([Function "y"; Lpar; Word "x"; Assign; Word "z"; Rpar;], Map [("y", [Word "x"]); ("x", [Number 2.0])])
        TestCaseData([Function "y"; Lpar; Word "x"; Assign; Number 4.0; Rpar;], Map [("y", [Word "x"; Exponent; Number 2.0; Plus; Word "z"])])
        TestCaseData([Function "y"; Lpar; Word "x"; Assign; Number 4.0; Comma;], Map [("y", [Word "x"; Exponent; Number 2.0; Plus; Word "z"])])
    ]

/// <summary>Test to ensure that exec correctly throws exception with invalid user function call.</summary>
[<TestCaseSource("UserFunctionErrorCases")>]
let GivenExec_WhenPassedInvalidUserFunctionCall_RaiseExecError(terminals: terminal list, env: Map<string, terminal list>) =
    Assert.Throws<ExecError>(fun () -> exec env terminals  |> ignore) |> ignore

//TEST DIFFERENTIATE
//ADD ERROR CASE FOR ASSIGN ASSIGN