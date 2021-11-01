module Interpreter.Tests.ExecTests

open System.Collections.Generic
open NUnit.Framework
open Interpreter.Exec
open Interpreter.Util
    
let CalculatePlusData =
    [
        TestCaseData(1, 6, Number 7.0)
        TestCaseData(0, 5, Number 5.0)
        TestCaseData(5, 0, Number 5.0)
        TestCaseData(0, 0, Number 0.0)
        TestCaseData(-10, -10, Number -20.0)
        TestCaseData(7, -7, Number 0.0)
        TestCaseData(-19, 19, Number 0.0)
    ]
    
let CalculateMinusData =
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
    
let CalculateTimesData =
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
    
let CalculateDivideData =
    [
        TestCaseData(1, 6, Number (1.0/6.0))
        TestCaseData(0, 5, Number 0.0)
        TestCaseData(5, 1, Number 5.0)
        TestCaseData(-10, -10, Number 1.0)
        TestCaseData(10, 10, Number 1.0)
        TestCaseData(7, -7, Number -1.0)
        TestCaseData(-19, 19, Number -1.0)
    ]
    
let CalculateExponentData =
    [
        TestCaseData(1, 6, Number 1.0)
        TestCaseData(0, 5, Number 0.0)
        TestCaseData(5, 0, Number 1.0)
        TestCaseData(0, 0, Number 1.0)
        TestCaseData(-10, -10, Number (-10.0 ** -10.0))
        TestCaseData(7, 2, Number 49.0)
        TestCaseData(-7, 2, Number 49.0)
    ]
    
let InvalidCalculateData =
    [
        TestCaseData(Lpar)
        TestCaseData(Rpar)
        TestCaseData(UnaryPlus)
        TestCaseData(UnaryMinus)
    ]
    
[<TestCaseSource("CalculatePlusData")>]
let GivenCalculate_WhenPassedSimpleAddition_ReturnCorrectAnswer(op1: float, op2: float, res: terminal) =
    let result = calculate Plus op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("CalculateMinusData")>]
let GivenCalculate_WhenPassedSimpleSubtraction_ReturnCorrectAnswer(op1: float, op2: float, res: terminal) =
    let result = calculate Minus op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("CalculateTimesData")>]
let GivenCalculate_WhenPassedSimpleMultiplication_ReturnCorrectAnswer(op1: float, op2: float, res: terminal) =
    let result = calculate Times op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("CalculateDivideData")>]
let GivenCalculate_WhenPassedSimpleDivision_ReturnCorrectAnswer(op1: float, op2: float, res: terminal) =
    let result = calculate Divide op1 op2
    Assert.That(result, Is.EqualTo(res))
    
    
[<TestCaseSource("CalculateExponentData")>]
let GivenCalculate_WhenPassedSimpleExponent_ReturnCorrectAnswer(op1: float, op2: float, res: terminal) =
    let result = calculate Exponent op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("InvalidCalculateData")>]
let GivenCalculate_WhenPassedInvalidOperator_RaiseCalculateError(operator: terminal) =
    Assert.Throws<CalculateError>(fun () -> calculate operator 1.0 1.0 |> ignore) |> ignore
    
[<Test>]
let GivenCalculate_WhenPassedDivideByZero_RaiseCalculateError() =
    Assert.Throws<CalculateError>(fun () -> calculate Divide 1.0 0.0 |> ignore) |> ignore
    
let ValidUnaryData =
    [
        TestCaseData(UnaryMinus, 1, Number -1.0)
        TestCaseData(UnaryMinus, -1, Number 1.0)
        TestCaseData(UnaryPlus, 1, Number 1.0)
        TestCaseData(UnaryPlus, -1, Number -1.0)
    ]
    
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
    
[<TestCaseSource("ValidUnaryData")>]
let GivenUnary_WhenPassedSimpleExpression_ReturnCorrectAnswer(op1: terminal, op2: float, res: terminal) =
    let result = unary op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("InvalidUnaryData")>]
let GivenUnary_WhenPassedInvalidOperator_RaiseUnaryError(operator: terminal) =
    Assert.Throws<UnaryError>(fun () -> unary operator 1.0 |> ignore) |> ignore
    
let ValidReduceCases =
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
    
[<TestCaseSource("ValidReduceCases")>]
let GivenReduce_WhenPassedValidTokens_ReturnCorrectAnswer(tokens: terminal list, expected: terminal) =
    let result = reduce tokens Map.empty
    Assert.That(result, Is.EqualTo(expected))
    
[<TestCaseSource("ValidReduceCases")>]
let GivenReduceRecursive_WhenPassedValidTokens_ReturnCorrectAnswer(tokens: terminal list, expected: terminal) =
    let result = reduceRecursive tokens [] [] Map.empty
    Assert.That(result, Is.EqualTo(expected))
    
let InvalidReduceCases =
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
    
[<TestCaseSource("InvalidReduceCases")>]
let GivenReduce_WhenPassedInvalidTokens_RaiseExecError(tokens: terminal list) =
    Assert.Throws<ExecError>(fun () -> reduce tokens Map.empty |> ignore) |> ignore
    
[<TestCaseSource("InvalidReduceCases")>]
let GivenReduceRecursive_WhenPassedInvalidTokens_RaiseExecError(tokens: terminal list) =
    Assert.Throws<ExecError>(fun () -> reduceRecursive tokens [] [] Map.empty |> ignore) |> ignore

let ValidPerformOperationCases =
    [
        TestCaseData([Plus;], [Number 1.0; Number 2.0;], (([] : terminal list), [Number 3.0;]))
        TestCaseData([Minus;], [Number 2.0; Number 4.0;], (([] : terminal list), [Number 2.0;]))
        TestCaseData([Times;], [Number 2.0; Number 4.0;], (([] : terminal list), [Number 8.0;]))
        TestCaseData([Divide;], [Number 2.0; Number 4.0;], (([] : terminal list), [Number 2.0;]))
        TestCaseData([Exponent;], [Number 2.0; Number 4.0;], (([] : terminal list), [Number 16.0;]))
        TestCaseData([UnaryPlus;], [Number 2.0;], (([] : terminal list), [Number 2.0;]))
        TestCaseData([UnaryMinus;], [Number 3.0;], (([] : terminal list), [Number -3.0;]))
        TestCaseData([Plus; Plus;], [Number 1.0; Number 2.0;], ([Plus;], [Number 3.0;]))
        TestCaseData([Minus; Plus;], [Number 2.0; Number 4.0;], ([Plus;], [Number 2.0;]))
        TestCaseData([Times; Plus;], [Number 2.0; Number 4.0;], ([Plus;], [Number 8.0;]))
        TestCaseData([Divide; Plus;], [Number 2.0; Number 4.0;], ([Plus;], [Number 2.0;]))
        TestCaseData([Exponent; Plus;], [Number 2.0; Number 4.0;], ([Plus;], [Number 16.0;]))
        TestCaseData([UnaryPlus; Plus;], [Number 2.0;], ([Plus;], [Number 2.0;]))
        TestCaseData([UnaryMinus; Plus;], [Number 3.0;], ([Plus;], [Number -3.0;]))
        TestCaseData([Plus;], [Number 1.0; Number 2.0; Number 2.0;], (([] : terminal list), [Number 3.0; Number 2.0;]))
        TestCaseData([Minus;], [Number 2.0; Number 4.0; Number 2.0;], (([] : terminal list), [Number 2.0; Number 2.0;]))
        TestCaseData([Times;], [Number 2.0; Number 4.0; Number 2.0;], (([] : terminal list), [Number 8.0; Number 2.0;]))
        TestCaseData([Divide;], [Number 2.0; Number 4.0; Number 2.0;], (([] : terminal list), [Number 2.0; Number 2.0;]))
        TestCaseData([Exponent;], [Number 2.0; Number 4.0; Number 2.0;], (([] : terminal list), [Number 16.0; Number 2.0;]))
        TestCaseData([UnaryPlus;], [Number 2.0; Number 2.0;], (([] : terminal list), [Number 2.0; Number 2.0;]))
        TestCaseData([UnaryMinus;], [Number 3.0; Number 2.0;], (([] : terminal list), [Number -3.0; Number 2.0;]))
    ]
    
[<TestCaseSource("ValidPerformOperationCases")>]
let GivenPerformOperation_WhenPassedValidInput_ReturnCorrectTuple(opList: terminal list, numList: terminal list, res: terminal list * terminal list) =
    let result = performOperation opList numList
    Assert.That(result, Is.EqualTo(res))
    
let InvalidPerformOperationsCases =
    [
        TestCaseData(([] : terminal list), ([] : terminal list))
        TestCaseData(([] : terminal list), [Number 1.0;])
        TestCaseData(([] : terminal list), [Number 1.0; Number 2.0;])
        TestCaseData([Rpar;], ([] : terminal list))
        TestCaseData([Rpar], [Number 1.0;])
        TestCaseData([Rpar], [Number 1.0; Number 2.0;])
        TestCaseData([Lpar;], ([] : terminal list))
        TestCaseData([Lpar], [Number 1.0;])
        TestCaseData([Lpar], [Number 1.0; Number 2.0;])
        TestCaseData([Plus;], ([] : terminal list))
        TestCaseData([Plus;], [Number 1.0;])
        TestCaseData([Minus;], ([] : terminal list))
        TestCaseData([Minus], [Number 1.0;])
        TestCaseData([Times;], ([] : terminal list))
        TestCaseData([Times;], [Number 1.0;])
        TestCaseData([Divide;], ([] : terminal list))
        TestCaseData([Divide], [Number 1.0;])
        TestCaseData([Exponent;], ([] : terminal list))
        TestCaseData([Exponent;], [Number 1.0;])
        TestCaseData([UnaryPlus;], ([] : terminal list))
        TestCaseData([UnaryMinus], ([] : terminal list))
    ]
    
[<TestCaseSource("InvalidPerformOperationsCases")>]
let GivenPerformOperations_WhenPassedIncompleteArguments_RaiseExecError(opList: terminal list, numList: terminal list) =
    Assert.Throws<ExecError>(fun () -> performOperation opList numList |> ignore) |> ignore
    
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
    
[<TestCaseSource("ValidGetPrecedenceData")>]
let GivenGetPrecedence_WhenPassedOperatorWithPrecedence_ReturnCorrectPrecedence(operator: terminal, precedence: int) =
    let result = getPrecedence operator
    Assert.That(result, Is.EqualTo(precedence))
    
[<TestCaseSource("ValidGetAssociativityData")>]
let GivenGetAssociativity_WhenPassedOperatorWithAssociativity_ReturnCorrectAssociativity(operator: terminal, associativity: string) =
    let result = getAssociativity operator
    Assert.That(result, Is.EqualTo(associativity))
    
let InvalidGetPrecedenceAssociativityCases =
    [
        TestCaseData(Lpar)
        TestCaseData(Rpar)
        TestCaseData(Number 1.0)
    ]

[<TestCaseSource("InvalidGetPrecedenceAssociativityCases")>]
let GivenGetPrecedence_WhenPassedOperatorNotInMap_RaiseKeyNotFoundException(operator: terminal) =
    Assert.Throws<KeyNotFoundException>(fun () -> getPrecedence operator |> ignore) |> ignore
    
[<TestCaseSource("InvalidGetPrecedenceAssociativityCases")>]
let GivenGetAssociativity_WhenPassedOperatorNotInMap_RaiseKeyNotFoundException(operator: terminal) =
    Assert.Throws<KeyNotFoundException>(fun () -> getAssociativity operator |> ignore) |> ignore
    
let ValidEvaluateBracketsCases =
    [
        TestCaseData([Lpar;], [Number 1.0; Number 2.0;], (([] : terminal list), [Number 1.0; Number 2.0;]))
        TestCaseData([Plus; Lpar;], [Number 1.0; Number 2.0;], (([] : terminal list), [Number 3.0;]))
        TestCaseData([Divide; Times; Plus; Lpar;], [Number 1.0; Number 2.0; Number 4.0; Number 4.0;], (([] : terminal list), [Number 12.0;]))
        TestCaseData([Lpar; Plus;], [Number 1.0; Number 2.0;], ([Plus;], [Number 1.0; Number 2.0;]))
        TestCaseData([Plus; Lpar; Minus], [Number 1.0; Number 2.0;], ([Minus;], [Number 3.0;]))
        TestCaseData([Divide; Times; Plus; Lpar; Plus; Times; Divide;], [Number 1.0; Number 2.0; Number 4.0; Number 4.0;], ([Plus; Times; Divide;], [Number 12.0;]))
    ]
    
[<TestCaseSource("ValidEvaluateBracketsCases")>]
let GivenEvaluateBrackets_WhenPassedValidBracketedExpression_ThenReturnCorrectTuple(opList: terminal list, numList: terminal list, outLists: terminal list * terminal list) =
    let result = evaluateBrackets opList numList
    Assert.That(result, Is.EqualTo(outLists))

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
    
[<TestCaseSource("InvalidEvaluateBracketsCases")>]
let GivenEvaluateBrackets_WhenPassedInvalidExpression_RaiseExecError(opList: terminal list, numList: terminal list) =
    Assert.Throws<ExecError>(fun () -> evaluateBrackets opList numList |> ignore) |> ignore