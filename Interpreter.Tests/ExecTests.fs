/// <summary>
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

/// <summary>Test cases for valid addition input to PerformBinaryOperation.</summary>
let PerformBinaryOperationPlusData =
    [
        TestCaseData(1, 6, Number 7.0)
        TestCaseData(0, 5, Number 5.0)
        TestCaseData(5, 0, Number 5.0)
        TestCaseData(0, 0, Number 0.0)
        TestCaseData(-10, -10, Number -20.0)
        TestCaseData(7, -7, Number 0.0)
        TestCaseData(-19, 19, Number 0.0)
    ]

/// <summary>Test cases for valid subtraction input to PerformBinaryOperation.</summary>
let PerformBinaryOperationMinusData =
    [
        TestCaseData(1, 6, Number -5.0)
        TestCaseData(0, 5, Number -5.0)
        TestCaseData(5, 0, Number 5.0)
        TestCaseData(0, 0, Number 0.0)
        TestCaseData(8, 8, Number 0.0)
        TestCaseData(-10, -10, Number 0.0)
        TestCaseData(7, -7, Number 14.0)
        TestCaseData(-19, 19, Number -38.0)
    ]

/// <summary>Test cases for valid multiplication input to PerformBinaryOperation.</summary>
let PerformBinaryOperationTimesData =
    [
        TestCaseData(1, 6, Number 6.0)
        TestCaseData(6, 1, Number 6.0)
        TestCaseData(0, 5, Number 0.0)
        TestCaseData(5, 0, Number 0.0)
        TestCaseData(0, 0, Number 0.0)
        TestCaseData(-10, -10, Number 100.0)
        TestCaseData(10, 10, Number 100.0)
        TestCaseData(10, -10, Number -100.0)
        TestCaseData(-10, 10, Number -100.0)
    ]

/// <summary>Test cases for valid division input to PerformBinaryOperation.</summary>
let PerformBinaryOperationDivideData =
    [
        TestCaseData(1, 6, Number (1.0/6.0))
        TestCaseData(0, 5, Number 0.0)
        TestCaseData(5, 1, Number 5.0)
        TestCaseData(-10, -10, Number 1.0)
        TestCaseData(10, 10, Number 1.0)
        TestCaseData(7, -7, Number -1.0)
        TestCaseData(-19, 19, Number -1.0)
    ]

/// <summary>Test cases for valid exponentiation input to PerformBinaryOperation.</summary>
let PerformBinaryOperationExponentData =
    [
        TestCaseData(1, 6, Number 1.0)
        TestCaseData(0, 5, Number 0.0)
        TestCaseData(5, 0, Number 1.0)
        TestCaseData(0, 0, Number 1.0)
        TestCaseData(-10, -10, Number (-10.0 ** -10.0))
        TestCaseData(7, 2, Number 49.0)
        TestCaseData(-7, 2, Number 49.0)
    ]

/// <summary>Test cases for invalid addition to PerformBinaryOperation.</summary>
let InvalidPerformBinaryOperationData =
    [
        TestCaseData(Lpar)
        TestCaseData(Rpar)
        TestCaseData(UnaryPlus)
        TestCaseData(UnaryMinus)
    ]

/// <summary>Test to ensure that performBinaryOperation performs correctly for additions.</summary>
[<TestCaseSource("PerformBinaryOperationPlusData")>]
let GivenPerformBinaryOperation_WhenPassedSimpleAddition_ReturnCorrectAnswer(op1: float, op2: float, res: terminal) =
    let result = performBinaryOperation Plus op1 op2
    Assert.That(result, Is.EqualTo(res))

/// <summary>Test to ensure that performBinaryOperation performs correctly for subtractions.</summary>
[<TestCaseSource("PerformBinaryOperationMinusData")>]
let GivenPerformBinaryOperation_WhenPassedSimpleSubtraction_ReturnCorrectAnswer(op1: float, op2: float, res: terminal) =
    let result = performBinaryOperation Minus op1 op2
    Assert.That(result, Is.EqualTo(res))

/// <summary>Test to ensure that performBinaryOperation performs correctly for multiplication.</summary>
[<TestCaseSource("PerformBinaryOperationTimesData")>]
let GivenPerformBinaryOperation_WhenPassedSimpleMultiplication_ReturnCorrectAnswer(op1: float, op2: float, res: terminal) =
    let result = performBinaryOperation Times op1 op2
    Assert.That(result, Is.EqualTo(res))

/// <summary>Test to ensure that performBinaryOperation performs correctly for divisions.</summary>
[<TestCaseSource("PerformBinaryOperationDivideData")>]
let GivenPerformBinaryOperation_WhenPassedSimpleDivision_ReturnCorrectAnswer(op1: float, op2: float, res: terminal) =
    let result = performBinaryOperation Divide op1 op2
    Assert.That(result, Is.EqualTo(res))

/// <summary>Test to ensure that performBinaryOperation performs correctly for exponentiation.</summary>
[<TestCaseSource("PerformBinaryOperationExponentData")>]
let GivenPerformBinaryOperation_WhenPassedSimpleExponent_ReturnCorrectAnswer(op1: float, op2: float, res: terminal) =
    let result = performBinaryOperation Exponent op1 op2
    Assert.That(result, Is.EqualTo(res))

/// <summary>Test to ensure that performBinaryOperation performs correctly for invalid input.</summary>
[<TestCaseSource("InvalidPerformBinaryOperationData")>]
let GivenPerformBinaryOperation_WhenPassedInvalidOperator_RaisePerformBinaryOperationError(operator: terminal) =
    Assert.Throws<CalculateError>(fun () -> performBinaryOperation operator 1.0 1.0 |> ignore) |> ignore

/// <summary>Test to ensure that performBinaryOperation performs correctly for dividing by 0.</summary>
[<Test>]
let GivenPerformBinaryOperation_WhenPassedDivideByZero_RaisePerformBinaryOperationError() =
    Assert.Throws<CalculateError>(fun () -> performBinaryOperation Divide 1.0 0.0 |> ignore) |> ignore

/// <summary>Test cases for valid input to calculateUnaryOperation.</summary>
let ValidUnaryData =
    [
        TestCaseData(UnaryMinus, 1, Number -1.0)
        TestCaseData(UnaryMinus, -1, Number 1.0)
        TestCaseData(UnaryPlus, 1, Number 1.0)
        TestCaseData(UnaryPlus, -1, Number -1.0)
    ]

/// <summary>Test cases for invalid input to calculateUnaryOperation.</summary>
let InvalidUnaryData =
    [
        TestCaseData(Plus)
        TestCaseData(Minus)
        TestCaseData(Times)
        TestCaseData(Divide)
        TestCaseData(Exponent)
        TestCaseData(Lpar)
        TestCaseData(Rpar)
    ]

/// <summary>Test to ensure that performUnaryOperation performs correctly for valid input.</summary>
[<TestCaseSource("ValidUnaryData")>]
let GivenUnary_WhenPassedSimpleExpression_ReturnCorrectAnswer(op1: terminal, op2: float, res: terminal) =
    let result = performUnaryOperation op1 op2
    Assert.That(result, Is.EqualTo(res))

/// <summary>Test to ensure that performUnaryOperation performs correctly for invalid input.</summary>
[<TestCaseSource("InvalidUnaryData")>]
let GivenUnary_WhenPassedInvalidOperator_RaiseUnaryError(operator: terminal) =
    Assert.Throws<UnaryError>(fun () -> performUnaryOperation operator 1.0 |> ignore) |> ignore

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

/// <summary>Test to ensure that reduce returns the correct output with valid input without variables.</summary>
[<TestCaseSource("ValidNoVariablesReduceCases")>]
let GivenReduce_WhenPassedValidTokensWithoutVariables_ReturnCorrectAnswer(tokens: terminal list, expected: terminal) =
    let result = reduce tokens Map.empty
    Assert.That(result, Is.EqualTo(expected))

/// <summary>
/// Test to ensure that reduceRecursive returns the correct output with valid input without variables.
/// </summary>
[<TestCaseSource("ValidNoVariablesReduceCases")>]
let GivenReduceRecursive_WhenPassedValidTokensWithoutVariables_ReturnCorrectAnswer(tokens: terminal list, expected: terminal) =
    let result = reduceRecursive tokens [] [] Map.empty
    Assert.That(result, Is.EqualTo(expected))

/// <summary>Test to ensure that exec returns the correct output with valid input without variables.</summary>
[<TestCaseSource("ValidNoVariablesReduceCases")>]
let GivenExec_WhenPassedSimpleExpressionWithoutVariables_ReturnCorrectAnswer(tokens: terminal list, expected: terminal) =
    let result = exec tokens Map.empty
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

/// <summary>Test to ensure that reduce returns the correct output with invalid input.</summary>
[<TestCaseSource("InvalidReduceCases")>]
let GivenReduce_WhenPassedInvalidTokens_RaiseExecError(tokens: terminal list) =
    Assert.Throws<ExecError>(fun () -> reduce tokens Map.empty |> ignore) |> ignore

/// <summary>Test to ensure that reduceRecursive returns the correct output with invalid input.</summary>
[<TestCaseSource("InvalidReduceCases")>]
let GivenReduceRecursive_WhenPassedInvalidTokens_RaiseExecError(tokens: terminal list) =
    Assert.Throws<ExecError>(fun () -> reduceRecursive tokens [] [] Map.empty |> ignore) |> ignore

/// <summary>Test to ensure that exec returns the correct output with invalid input.</summary>
[<TestCaseSource("InvalidReduceCases")>]
let GivenExed_WhenPassedInvalidTokens_RaiseExecError(tokens: terminal list) =
    Assert.Throws<ExecError>(fun () -> exec tokens Map.empty |> ignore) |> ignore

/// <summary>Test cases for valid input to performOperation.</summary>
let ValidPerformOperationCases =
    [
        TestCaseData(Plus, [Number 1.0; Number 2.0;], [Number 3.0;])
        TestCaseData(Minus, [Number 2.0; Number 4.0;], [Number 2.0;])
        TestCaseData(Times, [Number 2.0; Number 4.0;], [Number 8.0;])
        TestCaseData(Divide, [Number 2.0; Number 4.0;], [Number 2.0;])
        TestCaseData(Exponent, [Number 2.0; Number 4.0;], [Number 16.0;])
        TestCaseData(UnaryPlus, [Number 2.0;], [Number 2.0;])
        TestCaseData(UnaryMinus, [Number 3.0;], [Number -3.0;])
        TestCaseData(Plus, [Number 1.0; Number 2.0; Number 2.0;], [Number 3.0; Number 2.0;])
        TestCaseData(Minus, [Number 2.0; Number 4.0; Number 2.0;], [Number 2.0; Number 2.0;])
        TestCaseData(Times, [Number 2.0; Number 4.0; Number 2.0;], [Number 8.0; Number 2.0;])
        TestCaseData(Divide, [Number 2.0; Number 4.0; Number 2.0;], [Number 2.0; Number 2.0;])
        TestCaseData(Exponent, [Number 2.0; Number 4.0; Number 2.0;], [Number 16.0; Number 2.0;])
        TestCaseData(UnaryPlus, [Number 2.0; Number 2.0;], [Number 2.0; Number 2.0;])
        TestCaseData(UnaryMinus, [Number 3.0; Number 2.0;], [Number -3.0; Number 2.0;])
    ]

/// <summary>Test to ensure that performOperation performs correctly with valid input.</summary>
[<TestCaseSource("ValidPerformOperationCases")>]
let GivenPerformOperation_WhenPassedValidInput_ReturnCorrectTuple(operator: terminal, numList: terminal list, res: terminal list) =
    let result = performOperation operator numList
    Assert.That(result, Is.EqualTo(res))

/// <summary>Test cases for invalid input to performOperation.</summary>
let InvalidPerformOperationsCases =
    [
        TestCaseData(Rpar, ([] : terminal list))
        TestCaseData(Rpar, [Number 1.0;])
        TestCaseData(Rpar, [Number 1.0; Number 2.0;])
        TestCaseData(Lpar, ([] : terminal list))
        TestCaseData(Lpar, [Number 1.0;])
        TestCaseData(Lpar, [Number 1.0; Number 2.0;])
        TestCaseData(Plus, ([] : terminal list))
        TestCaseData(Plus, [Number 1.0;])
        TestCaseData(Minus, ([] : terminal list))
        TestCaseData(Minus, [Number 1.0;])
        TestCaseData(Times, ([] : terminal list))
        TestCaseData(Times, [Number 1.0;])
        TestCaseData(Divide, ([] : terminal list))
        TestCaseData(Divide, [Number 1.0;])
        TestCaseData(Exponent, ([] : terminal list))
        TestCaseData(Exponent, [Number 1.0;])
        TestCaseData(UnaryPlus, ([] : terminal list))
        TestCaseData(UnaryMinus, ([] : terminal list))
    ]

/// <summary>Test to ensure that performOperation performs correctly with valid input.</summary>
[<TestCaseSource("InvalidPerformOperationsCases")>]
let GivenPerformOperations_WhenPassedIncompleteArguments_RaiseExecError(operator: terminal, numList: terminal list) =
    Assert.Throws<ExecError>(fun () -> performOperation operator numList |> ignore) |> ignore

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

/// <summary>Test cases for valid input to evaluateBrackets.</summary>
let ValidEvaluateBracketsCases =
    [
        TestCaseData([Lpar;], [Number 1.0; Number 2.0;], (([] : terminal list), [Number 1.0; Number 2.0;]))
        TestCaseData([Plus; Lpar;], [Number 1.0; Number 2.0;], (([] : terminal list), [Number 3.0;]))
        TestCaseData([Divide; Times; Plus; Lpar;], [Number 1.0; Number 2.0; Number 4.0; Number 4.0;], (([] : terminal list), [Number 12.0;]))
        TestCaseData([Lpar; Plus;], [Number 1.0; Number 2.0;], ([Plus;], [Number 1.0; Number 2.0;]))
        TestCaseData([Plus; Lpar; Minus], [Number 1.0; Number 2.0;], ([Minus;], [Number 3.0;]))
        TestCaseData([Divide; Times; Plus; Lpar; Plus; Times; Divide;], [Number 1.0; Number 2.0; Number 4.0; Number 4.0;], ([Plus; Times; Divide;], [Number 12.0;]))
    ]

/// <summary>Test to ensure that evaluateBrackets performs correctly under valid input.</summary>
[<TestCaseSource("ValidEvaluateBracketsCases")>]
let GivenEvaluateBrackets_WhenPassedValidBracketedExpression_ThenReturnCorrectTuple(opList: terminal list, numList: terminal list, outLists: terminal list * terminal list) =
    let result = evaluateBrackets opList numList
    Assert.That(result, Is.EqualTo(outLists))

/// <summary>Test cases for invalid input to evaluateBrackets.</summary>
let InvalidEvaluateBracketsCases =
    [
        TestCaseData(([] : terminal list), ([] : terminal list))
        TestCaseData(([] : terminal list), [Number 1.0;])
        TestCaseData(([] : terminal list), [Number 1.0; Number 2.0;])
        TestCaseData([Rpar;], ([] : terminal list))
        TestCaseData([Rpar;], [Number 1.0;])
        TestCaseData([Rpar;], [Number 1.0; Number 2.0;])
        TestCaseData([UnaryMinus; Lpar;], ([] : terminal list))
        TestCaseData([Plus; Lpar;], [Number 1.0;])
        TestCaseData([Plus; Times; Divide; Lpar;], [Number 1.0; Number 2.0; Number 4.0;])
        TestCaseData([Plus;], [Number 1.0; Number 2.0;])
        TestCaseData([Divide; Times; Plus;], [Number 1.0; Number 2.0; Number 4.0; Number 4.0;])
    ]

/// <summary>Test to ensure that evaluateBrackets correctly throws exception with invalid input.</summary>
[<TestCaseSource("InvalidEvaluateBracketsCases")>]
let GivenEvaluateBrackets_WhenPassedInvalidExpression_RaiseExecError(opList: terminal list, numList: terminal list) =
    Assert.Throws<ExecError>(fun () -> evaluateBrackets opList numList |> ignore) |> ignore

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
    let result = closed terminals env
    Assert.That(result, Is.EqualTo(expected))

/// <summary>Test cases for valid input to reduce and reduceRecursive with variables.</summary>
let ValidVariablesReduceCases =
    [
        TestCaseData([Word "x"], Number 2.0)
        TestCaseData([Word "x"; Plus; Number 1.0], Number 3.0)
        TestCaseData([Number 1.0; Plus; Word "x"], Number 3.0)
        TestCaseData([Word "x"; Plus; Word "x"], Number 4.0)
        TestCaseData([Word "y"], Number 3.0)
        TestCaseData([Word "x"; Times; Word "y"], Number 6.0)
        TestCaseData([Word "x"; Times; Word "y"; Times; Word "y"], Number 18.0)
    ]

/// <summary>Test to ensure that reduce returns correct output with valid input containing bound variables.</summary>
[<TestCaseSource("ValidVariablesReduceCases")>]
let GivenReduce_WhenPassedValidExpressionWithVariables_ReturnCorrectResult(terminals: terminal list, expected: terminal) =
    let result = reduce terminals env
    Assert.That(result, Is.EqualTo(expected))

/// <summary>
/// Test to ensure that reduceRecursive returns correct output with valid input containing bound variables.
/// </summary>
[<TestCaseSource("ValidVariablesReduceCases")>]
let GivenReduceRecursive_WhenPassedValidExpressionWithVariables_ReturnCorrectResult(terminals: terminal list, expected: terminal) =
    let result = reduceRecursive terminals [] [] env
    Assert.That(result, Is.EqualTo(expected))

/// <summary>Test cases for invalid input to reduce and reduceRecursive with variables.</summary>
let InvalidVariablesReduceCases =
    [
        TestCaseData([Word "z"])
        TestCaseData([Word "x"; Plus; Word "z"])
        TestCaseData([Word "z"; Plus; Word "x"])
        TestCaseData([Word "a"])
        TestCaseData([Word "x"; Plus; Word "a"])
    ]

/// <summary>Test to ensure that reduce correctly throws exception with invalid input with variables.</summary>
[<TestCaseSource("InvalidReduceCases")>]
let GivenReduce_WhenPassedInvalidTokensWithVariables_RaiseExecError(tokens: terminal list) =
    Assert.Throws<ExecError>(fun () -> reduce tokens env |> ignore) |> ignore

/// <summary>Test to ensure that reduceRecursive correctly throws exception with invalid input with variables.</summary>
[<TestCaseSource("InvalidReduceCases")>]
let GivenReduceRecursive_WhenPassedInvalidTokensWithVariables_RaiseExecError(tokens: terminal list) =
    Assert.Throws<ExecError>(fun () -> reduceRecursive tokens [] [] env |> ignore) |> ignore

/// <summary>Test cases for valid assign inputs to exec.</summary>
let ValidExecAssignCases =
    [
        TestCaseData([Word "x"; Assign; Number 2.0], [Number 2.0], ("x", [Number 2.0]))
        TestCaseData([Word "x"; Assign; Word "x";], [Number 2.0], ("x", [Number 2.0]))
        TestCaseData([Word "x"; Assign; Word "x"; Plus; Word "x"], [Number 4.0], ("x", [Number 4.0]))
        TestCaseData([Word "y"; Assign; Word "z"], [Word "y"; Assign; Word "z"], ("y", [Word "z"]))
        TestCaseData([Word "b"; Assign; Word "x"; Times; Word "y"], [Number 6.0], ("b", [Number 6.0]))
        TestCaseData([Word "x"; Assign; Word "x"; Plus; Number 1.0], [Number 3.0], ("x", [Number 3.0]))
    ]

/// <summary>Test to ensure that exec correctly updates environment when passed valid assign.</summary>
[<TestCaseSource("ValidExecAssignCases")>]
let GivenExec_WhenPassedValidAssign_ThenAddToEnvAndReturn(terminals: terminal list, reduction: terminal list, entry: string*terminal list) =
    let result = exec terminals env
    Assert.That(result, Is.EqualTo((reduction, (env.Add entry))))

/// <summary>Test cases for valid input to createTerminalListUpToComma.</summary>
let CreateTerminalListUpToCommaCases =
    [
        TestCaseData([Rpar], ([] : terminal list), ([Rpar], ([] : terminal list)))
        TestCaseData([Rpar], [Number 2.0; Assign; Word "x"], ([Rpar], [Word "x"; Assign; Number 2.0]))
        TestCaseData([Word "x"; Assign; Number 2.0; Rpar], ([] : terminal list), ([Rpar], [Word "x"; Assign; Number 2.0]))
        TestCaseData([Comma], ([] : terminal list), (([] : terminal list), ([] : terminal list)))
        TestCaseData([Comma], [Number 2.0; Assign; Word "x"], (([] : terminal list), [Word "x"; Assign; Number 2.0]))
        TestCaseData([Word "x"; Assign; Number 2.0; Comma], ([] : terminal list), (([] : terminal list), [Word "x"; Assign; Number 2.0]))
        TestCaseData([Comma; Word "y"; Assign; Number 3.0], ([] : terminal list), ([Word "y"; Assign; Number 3.0], ([] : terminal list)))
        TestCaseData([Comma; Word "y"; Assign; Number 3.0], [Number 2.0; Assign; Word "x"], ([Word "y"; Assign; Number 3.0], [Word "x"; Assign; Number 2.0]))
    ]

/// <summary>Test to ensure that createTerminalListUpToComma forms correct terminal list with valid input.</summary>
[<TestCaseSource("CreateTerminalListUpToCommaCases")>]
let GivenCreateTerminalListUpToComma_WhenPassedTokens_ReadUpToCommaOrRparCorrectly(terminalIn: terminal list, terminalsOut: terminal list, expected: terminal list * terminal list) =
    let result = extractAssignment terminalIn terminalsOut
    Assert.That(result, Is.EqualTo(expected))

/// <summary>Test cases for invalid input to createTerminalListUpToComma.</summary>
let CreateTerminalListUpToCommaErrorCases =
    [
        TestCaseData([] : terminal list)
        TestCaseData([Word "x"; Assign; Number 2.0;])
    ]

/// <summary>Test to ensure that createTerminalListUpToComma correctly throws exception with invalid input.</summary>
[<TestCaseSource("CreateTerminalListUpToCommaErrorCases")>]
let GivenCreateTerminalListUpToComma_WhenPassedEmptyArray_RaiseExecError(terminalIn: terminal list) =
    Assert.Throws<ExecError>(fun () -> extractAssignment terminalIn [] |> ignore) |> ignore

/// <summary>Test cases for valid input to setArguments.</summary>
let SetArgumentsCases =
    [
        TestCaseData([Rpar], Map [("x", [Number 2.0])], Map [("x", [Number 2.0])])
        TestCaseData([Word "x"; Assign; Number 2.0; Rpar], Map.empty<string, terminal list>, Map [("x", [Number 2.0])])
        TestCaseData([Word "x"; Assign; Number 2.0; Comma; Word "y"; Assign; Number 3.0; Rpar], Map.empty<string, terminal list>, Map [("x", [Number 2.0]); ("y", [Number 3.0])])
        TestCaseData([Word "x"; Assign; Number 2.0; Rpar], Map [("z", [Number 4.0])], Map [("x", [Number 2.0]); ("z", [Number 4.0])])
        TestCaseData([Word "x"; Assign; Number 2.0; Comma; Word "y"; Assign; Number 3.0; Rpar], Map [("z", [Number 4.0])], Map [("x", [Number 2.0]); ("y", [Number 3.0]); ("z", [Number 4.0])])
        TestCaseData([Word "x"; Assign; Number 2.0; Rpar], Map [("x", [Number 4.0])], Map [("x", [Number 2.0])])
        TestCaseData([Word "x"; Assign; Number 2.0; Comma; Word "y"; Assign; Number 3.0; Rpar], Map [("x", [Number 4.0])], Map [("x", [Number 2.0]); ("y", [Number 3.0])])
    ]

/// <summary>Test to ensure that set arguments creates the correct map when passed valid input.</summary>
[<TestCaseSource("SetArgumentsCases")>]
let GivenSetArguments_WhenPassedValidAssignments_ReturnUpdatedMap(terminals: terminal list, inMap: Map<string, terminal list>, outMap: Map<string, terminal list>) =
    let result = setArguments terminals inMap
    Assert.That(result, Is.EqualTo(outMap))

/// <summary>Test cases for invalid input to setArguments.</summary>
let SetArgumentsErrorCases =
    [
        TestCaseData([] : terminal list)
        TestCaseData([Comma;])
        TestCaseData([Plus;])
        TestCaseData([Minus;])
        TestCaseData([Times;])
        TestCaseData([Divide;])
        TestCaseData([Exponent;])
        TestCaseData([Lpar])
        TestCaseData([UnaryPlus])
        TestCaseData([UnaryMinus])
        TestCaseData([Number 1.0])
        TestCaseData([Word "x"])
        TestCaseData([Assign;])
        TestCaseData([Word "x"; Assign])
    ]

/// <summary>Test to ensure that setArguments correctly throws exception with invalid input.</summary>
[<TestCaseSource("SetArgumentsErrorCases")>]
let GivenSetArguments_WhenInvalidAssignment_RaiseExecError(terminalIn: terminal list) =
    Assert.Throws<ExecError>(fun () -> setArguments terminalIn Map.empty<string, terminal list> |> ignore) |> ignore

/// <summary>Test cases for valid user function calls to exec.</summary>
let UserFunctionCases =
    [
        TestCaseData([Word "y"; Lpar; Rpar;], Map [("y", [Number 2.0])], [Number 2.0])
        TestCaseData([Word "y"; Lpar; Rpar;], Map [("y", [Word "x"]); ("x", [Number 2.0])], [Number 2.0])
        TestCaseData([Word "y"; Lpar; Word "x"; Assign; Number 2.0; Rpar;], Map [("y", [Word "x"]); ("x", [Number 2.0])], [Number 2.0])
        TestCaseData([Word "y"; Lpar; Word "x"; Assign; Number 4.0; Rpar;], Map [("y", [Word "x"])], [Number 4.0])
        TestCaseData([Word "y"; Lpar; Word "x"; Assign; Number 4.0; Rpar;], Map [("y", [Word "x"; Exponent; Number 2.0])], [Number 16.0])
        TestCaseData([Word "y"; Lpar; Word "x"; Assign; Number 4.0; Comma; Word "z"; Assign; Number 3.0; Rpar;], Map [("y", [Word "x"; Exponent; Number 2.0; Plus; Word "z"])], [Number 19.0])
    ]

/// <summary>Test to ensure that exec returns correctly result with valid user function call.</summary>
[<TestCaseSource("UserFunctionCases")>]
let GivenExec_WhenPassedValidUserFunctionCall_ReturnCorrectResult(terminals: terminal list, env: Map<string, terminal list>, expected: terminal list) =
    let result = exec terminals env
    Assert.That(result, Is.EqualTo((expected, env |> Map.toSeq |> dict)))

/// <summary>Test cases for invalid input to exec with user functions.</summary>
let UserFunctionErrorCases =
    [
        TestCaseData([Word "y"; Lpar;], Map [("y", [Number 2.0])])
        TestCaseData([Word "y"; Lpar; Comma; Rpar;], Map [("y", [Word "x"])])
        TestCaseData([Word "y"; Lpar; Rpar;], Map [("y", [Word "x"]); ("x", [Word "z"])])
        TestCaseData([Word "y"; Lpar; Word "x"; Assign; Word "z"; Rpar;], Map [("y", [Word "x"]); ("x", [Number 2.0])])
        TestCaseData([Word "y"; Lpar; Word "x"; Assign; Number 4.0; Rpar;], Map [("y", [Word "x"; Exponent; Number 2.0; Plus; Word "z"])])
        TestCaseData([Word "y"; Lpar; Word "x"; Assign; Number 4.0; Comma;], Map [("y", [Word "x"; Exponent; Number 2.0; Plus; Word "z"])])
    ]

/// <summary>Test to ensure that exec correctly throws exception with invalid user function call.</summary>
[<TestCaseSource("UserFunctionErrorCases")>]
let GivenExec_WhenPassedInvalidUserFunctionCall_RaiseExecError(terminals: terminal list, env: Map<string, terminal list>) =
    Assert.Throws<ExecError>(fun () -> exec terminals env |> ignore) |> ignore
    
//TEST DIFFERENTIATE
//ADD ERROR CASE FOR ASSIGN ASSIGN