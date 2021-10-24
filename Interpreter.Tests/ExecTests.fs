module Interpreter.Tests.ExecTests

open System.Collections.Generic
open NUnit.Framework
open Interpreter.Exec
open Interpreter.Util
    
let CalculatePlusData =
    [
        TestCaseData(1, 6, 7.0)
        TestCaseData(0, 5, 5.0)
        TestCaseData(5, 0, 5.0)
        TestCaseData(0, 0, 0.0)
        TestCaseData(-10, -10, -20.0)
        TestCaseData(7, -7, 0.0)
        TestCaseData(-19, 19, 0.0)
    ]
    
let CalculateMinusData =
    [
        TestCaseData(1, 6, -5.0)
        TestCaseData(0, 5, -5.0)
        TestCaseData(5, 0, 5.0)
        TestCaseData(0, 0, 0.0)
        TestCaseData(8, 8, 0.0)
        TestCaseData(-10, -10, 0.0)
        TestCaseData(7, -7, 14.0)
        TestCaseData(-19, 19, -38.0)
    ]
    
let CalculateTimesData =
    [
        TestCaseData(1, 6, 6.0)
        TestCaseData(6, 1, 6.0)
        TestCaseData(0, 5, 0.0)
        TestCaseData(5, 0, 0.0)
        TestCaseData(0, 0, 0.0)
        TestCaseData(-10, -10, 100.0)
        TestCaseData(10, 10, 100.0)
        TestCaseData(10, -10, -100)
        TestCaseData(-10, 10, -100)
    ]
    
let CalculateDivideData =
    [
        TestCaseData(1, 6, 1.0/6.0)
        TestCaseData(0, 5, 0)
        TestCaseData(5, 1, 5.0)
        TestCaseData(-10, -10, 1.0)
        TestCaseData(10, 10, 1.0)
        TestCaseData(7, -7, -1.0)
        TestCaseData(-19, 19, -1.0)
    ]
    
let CalculateExponentData =
    [
        TestCaseData(1, 6, 1)
        TestCaseData(0, 5, 0)
        TestCaseData(5, 0, 1)
        TestCaseData(0, 0, 1)
        TestCaseData(-10, -10, -10.0 ** -10.0)
        TestCaseData(7, 2, 49.0)
        TestCaseData(-7, 2, 49.0)
    ]
    
let InvalidCalculateData =
    [
        TestCaseData(Lpar)
        TestCaseData(Rpar)
        TestCaseData(UnaryPlus)
        TestCaseData(UnaryMinus)
    ]
    
[<TestCaseSource("CalculatePlusData")>]
let GivenCalculate_WhenPassedSimpleAddition_ReturnCorrectAnswer(op1: float, op2: float, res: float) =
    let result = calculate Plus op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("CalculateMinusData")>]
let GivenCalculate_WhenPassedSimpleSubtraction_ReturnCorrectAnswer(op1: float, op2: float, res: float) =
    let result = calculate Minus op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("CalculateTimesData")>]
let GivenCalculate_WhenPassedSimpleMultiplication_ReturnCorrectAnswer(op1: float, op2: float, res: float) =
    let result = calculate Times op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("CalculateDivideData")>]
let GivenCalculate_WhenPassedSimpleDivision_ReturnCorrectAnswer(op1: float, op2: float, res: float) =
    let result = calculate Divide op1 op2
    Assert.That(result, Is.EqualTo(res))
    
    
[<TestCaseSource("CalculateExponentData")>]
let GivenCalculate_WhenPassedSimpleExponent_ReturnCorrectAnswer(op1: float, op2: float, res: float) =
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
        TestCaseData(UnaryMinus, 1, -1)
        TestCaseData(UnaryMinus, -1, 1)
        TestCaseData(UnaryPlus, 1, 1)
        TestCaseData(UnaryPlus, -1, -1)
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
let GivenUnary_WhenPassedSimpleExpression_ReturnCorrectAnswer(op1: terminal, op2: float, res: float) =
    let result = unary op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("InvalidUnaryData")>]
let GivenUnary_WhenPassedInvalidOperator_RaiseUnaryError(operator: terminal) =
    Assert.Throws<UnaryError>(fun () -> unary operator 1.0 |> ignore) |> ignore
    
let ValidReduceCases =
    [
        //SIMPLE ADDITION CASES
        TestCaseData([Number 1.0; Plus; Number 1.0], 2.0)
        TestCaseData([Number 1.0; Plus; Number 0.0], 1.0)
        TestCaseData([Number 0.0; Plus; Number 67.0], 67.0)
        TestCaseData([Number 0.0; Plus; Number 0.0], 0.0)
        TestCaseData([Number 5.0; Plus; Number -5.0], 0.0)
        TestCaseData([Number -5.0; Plus; Number -5.0], -10.0)
        TestCaseData([Number -5.0; Plus; Number 5.0], 0.0)
        TestCaseData([Number 10.0; Plus; Number -100.0], -90.0)
        //SIMPLE SUBTRACTION CASES
        TestCaseData([Number 1.0; Minus; Number 1.0], 0.0)
        TestCaseData([Number 1.0; Minus; Number 0.0], 1.0)
        TestCaseData([Number 0.0; Minus; Number 67.0], -67.0)
        TestCaseData([Number 0.0; Minus; Number 0.0], 0.0)
        TestCaseData([Number 5.0; Minus; Number -5.0], 10.0)
        TestCaseData([Number -5.0; Minus; Number -5.0], 0.0)
        TestCaseData([Number -5.0; Minus; Number 5.0], -10.0)
        TestCaseData([Number 10.0; Minus; Number -100.0], 110.0)
        //SIMPLE MULTIPLY CASES
        TestCaseData([Number 1.0; Times; Number 8.0], 8.0)
        TestCaseData([Number 8.0; Times; Number 1.0], 8.0)
        TestCaseData([Number 0.0; Times; Number 11.0], 0.0)
        TestCaseData([Number 10.0; Times; Number 0.0], 0.0)
        TestCaseData([Number 0.0; Times; Number 0.0], 0.0)
        TestCaseData([Number -10.0; Times; Number -10.0], 100.0)
        TestCaseData([Number 10.0; Times; Number 10.0], 100.0)
        TestCaseData([Number -10.0; Times; Number 10.0], -100.0)
        TestCaseData([Number 10.0; Times; Number -10.0], -100.0)
        //SIMPLE DIVIDE CASES
        TestCaseData([Number 1.0; Divide; Number 8.0], 0.125)
        TestCaseData([Number 8.0; Divide; Number 1.0], 8.0)
        TestCaseData([Number 0.0; Divide; Number 11.0], 0.0)
        TestCaseData([Number -10.0; Divide; Number -10.0], 1.0)
        TestCaseData([Number 10.0; Divide; Number 10.0], 1.0)
        TestCaseData([Number -10.0; Divide; Number 10.0], -1.0)
        TestCaseData([Number 10.0; Divide; Number -10.0], -1.0)
        //SIMPLE EXPONENT CASES
        TestCaseData([Number 1.0; Exponent; Number 8.0], 1.0)
        TestCaseData([Number 8.0; Exponent; Number 1.0], 8.0)
        TestCaseData([Number 0.0; Exponent; Number 11.0], 0.0)
        TestCaseData([Number 11.0; Exponent; Number 0.0], 1.0)
        TestCaseData([Number 0.0; Exponent; Number 0.0], 1.0)
        TestCaseData([Number -10.0; Exponent; Number -10.0], -10.0 ** -10.00)
        TestCaseData([Number 10.0; Exponent; Number 2.0], 100.0)
        TestCaseData([Number -10.0; Exponent; Number 2.0], 100.0)
        TestCaseData([Number 10.0; Exponent; Number 3.0], 1000.0)
        TestCaseData([Number -10.0; Exponent; Number 3.0], -1000.0)
        TestCaseData([Number 10.0; Exponent; Number -3.0], 0.001)
        TestCaseData([Number -10.0; Exponent; Number -3.0], -0.001)
        //SIMPLE UNARY CASES
        TestCaseData([UnaryMinus; Number 1.0], -1.0)
        TestCaseData([UnaryMinus; Number -1.0], 1.0)
        TestCaseData([UnaryPlus; Number 1.0], 1.0)
        TestCaseData([UnaryPlus; Number -1.0], -1.0)
        //CHAINED UNARY CASES
        TestCaseData([UnaryMinus; UnaryMinus; Number 1.0], 1.0)
        TestCaseData([UnaryMinus; UnaryMinus; Number -1.0], -1.0)
        TestCaseData([UnaryPlus; UnaryPlus; Number 1.0], 1.0)
        TestCaseData([UnaryPlus; UnaryPlus; Number -1.0], -1.0)
        //ORDER OF OPERATIONS CASES
        TestCaseData([Number 1.0; Plus; Number 8.0; Minus; Number 6.0], 3.0)
        TestCaseData([Number 1.0; Minus; Number 8.0; Plus; Number 6.0], -1.0)
        TestCaseData([Number 64.0; Times; Number 8.0; Divide; Number 4.0], 128.0)
        TestCaseData([Number 64.0; Divide; Number 4.0; Times; Number 8.0], 128.0)
        TestCaseData([Number 7.0; Plus; Number 8.0; Times; Number 6.0], 55.0)
        TestCaseData([Number 9.0; Times; Number 8.0; Plus; Number 6.0], 78.0)
        TestCaseData([Number 6.0; Times; Number 8.0; Minus; Number 4.0], 44.0)
        TestCaseData([Number 6.0; Minus; Number 4.0; Times; Number 8.0], -26.0)
        TestCaseData([Number 1.0; Plus; Number 8.0; Divide; Number 4.0], 3.0)
        TestCaseData([Number 1.0; Divide; Number 8.0; Plus; Number 6.0], 6.125)
        TestCaseData([Number 64.0; Divide; Number 8.0; Minus; Number 4.0], 4.0)
        TestCaseData([Number 64.0; Minus; Number 4.0; Divide; Number 8.0], 63.5)
        TestCaseData([Number 1.0; Plus; Number 8.0; Exponent; Number 2.0], 65.0)
        TestCaseData([Number 1.0; Exponent; Number 8.0; Plus; Number 6.0], 7.0)
        TestCaseData([Number 2.0; Minus; Number 8.0; Exponent; Number 2.0], -62.0)
        TestCaseData([Number 16.0; Exponent; Number 2.0; Minus; Number 8.0], 248.0)
        TestCaseData([Number 7.0; Times; Number 5.0; Exponent; Number 3.0], 875.0)
        TestCaseData([Number 9.0; Exponent; Number 2.0; Times; Number 6.0], 486.0)
        TestCaseData([Number 64.0; Divide; Number 8.0; Exponent; Number 2.0], 1.0)
        TestCaseData([Number 6.0; Exponent; Number 3.0; Divide; Number 2.0], 108.0)
        TestCaseData([Lpar; Number 1.0; Plus; Number 8.0; Rpar; Minus; Number 6.0], 3.0)
        TestCaseData([Lpar; Number 1.0; Minus; Number 8.0; Rpar; Plus; Number 6.0], -1.0)
        TestCaseData([Lpar; Number 64.0; Times; Number 8.0; Rpar; Divide; Number 4.0], 128.0)
        TestCaseData([Lpar; Number 64.0; Divide; Number 4.0; Rpar; Times; Number 8.0], 128.0)
        TestCaseData([Lpar; Number 7.0; Plus; Number 8.0; Rpar; Times; Number 6.0], 90.0)
        TestCaseData([Lpar; Number 9.0; Times; Number 8.0; Rpar; Plus; Number 6.0], 78.0)
        TestCaseData([Lpar; Number 6.0; Times; Number 8.0; Rpar; Minus; Number 4.0], 44.0)
        TestCaseData([Lpar; Number 6.0; Minus; Number 4.0; Rpar; Times; Number 8.0], 16.0)
        TestCaseData([Lpar; Number 1.0; Plus; Number 8.0; Rpar; Divide; Number 4.0], 2.25)
        TestCaseData([Lpar; Number 1.0; Divide; Number 8.0; Rpar; Plus; Number 6.0], 6.125)
        TestCaseData([Lpar; Number 64.0; Divide; Number 8.0; Rpar; Minus; Number 4.0], 4.0)
        TestCaseData([Lpar; Number 64.0; Minus; Number 4.0; Rpar; Divide; Number 8.0], 7.5)
        TestCaseData([Lpar; Number 1.0; Plus; Number 8.0; Rpar; Exponent; Number 2.0], 81.0)
        TestCaseData([Lpar; Number 1.0; Exponent; Number 8.0; Rpar; Plus; Number 6.0], 7.0)
        TestCaseData([Lpar; Number 2.0; Minus; Number 8.0; Rpar; Exponent; Number 2.0], 36.0)
        TestCaseData([Lpar; Number 16.0; Exponent; Number 2.0; Rpar; Minus; Number 8.0], 248.0)
        TestCaseData([Lpar; Number 7.0; Times; Number 5.0; Rpar; Exponent; Number 3.0], 42875.0)
        TestCaseData([Lpar; Number 9.0; Exponent; Number 2.0; Rpar; Times; Number 6.0], 486.0)
        TestCaseData([Lpar; Number 64.0; Divide; Number 8.0; Rpar; Exponent; Number 2.0], 64.0)
        TestCaseData([Lpar; Number 6.0; Exponent; Number 3.0; Rpar; Divide; Number 2.0], 108.0)
        TestCaseData([UnaryMinus; Lpar; UnaryPlus; Number 6.0; Exponent; UnaryMinus; Number 3.0; Rpar; Divide; Number 2.0], ((-1.0/6.0**3.0)/2.0))
        TestCaseData([UnaryMinus; Lpar; UnaryPlus; Number 6.0; Divide; UnaryMinus; Number 3.0; Rpar; Exponent; Number 2.0], 4)
        TestCaseData([UnaryMinus; Number 2.0; Exponent; Number 3.0], -8)
    ]
    
[<TestCaseSource("ValidReduceCases")>]
let GivenReduce_WhenPassedValidTokens_ReturnCorrectAnswer(tokens: terminal list, expected: float) =
    let result = reduce tokens
    Assert.That(result, Is.EqualTo(expected))
    
[<TestCaseSource("ValidReduceCases")>]
let GivenReduceRecursive_WhenPassedValidTokens_ReturnCorrectAnswer(tokens: terminal list, expected: float) =
    let result = reduceRecursive tokens [] []
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
    Assert.Throws<ExecError>(fun () -> reduce tokens |> ignore) |> ignore
    
[<TestCaseSource("InvalidReduceCases")>]
let GivenReduceRecursive_WhenPassedInvalidTokens_RaiseExecError(tokens: terminal list) =
    Assert.Throws<ExecError>(fun () -> reduceRecursive tokens [] [] |> ignore) |> ignore

let ValidPerformOperationCases =
    [
        TestCaseData([Plus;], [1.0; 2.0;], (([] : terminal list), [3.0;]))
        TestCaseData([Minus;], [2.0; 4.0;], (([] : terminal list), [2.0;]))
        TestCaseData([Times;], [2.0; 4.0;], (([] : terminal list), [8.0;]))
        TestCaseData([Divide;], [2.0; 4.0;], (([] : terminal list), [2.0;]))
        TestCaseData([Exponent;], [2.0; 4.0;], (([] : terminal list), [16.0;]))
        TestCaseData([UnaryPlus;], [2.0;], (([] : terminal list), [2.0;]))
        TestCaseData([UnaryMinus;], [3.0;], (([] : terminal list), [-3.0;]))
        TestCaseData([Plus; Plus;], [1.0; 2.0;], ([Plus;], [3.0;]))
        TestCaseData([Minus; Plus;], [2.0; 4.0;], ([Plus;], [2.0;]))
        TestCaseData([Times; Plus;], [2.0; 4.0;], ([Plus;], [8.0;]))
        TestCaseData([Divide; Plus;], [2.0; 4.0;], ([Plus;], [2.0;]))
        TestCaseData([Exponent; Plus;], [2.0; 4.0;], ([Plus;], [16.0;]))
        TestCaseData([UnaryPlus; Plus;], [2.0;], ([Plus;], [2.0;]))
        TestCaseData([UnaryMinus; Plus;], [3.0;], ([Plus;], [-3.0;]))
        TestCaseData([Plus;], [1.0; 2.0; 2.0;], (([] : terminal list), [3.0; 2.0;]))
        TestCaseData([Minus;], [2.0; 4.0; 2.0;], (([] : terminal list), [2.0; 2.0;]))
        TestCaseData([Times;], [2.0; 4.0; 2.0;], (([] : terminal list), [8.0; 2.0;]))
        TestCaseData([Divide;], [2.0; 4.0; 2.0;], (([] : terminal list), [2.0; 2.0;]))
        TestCaseData([Exponent;], [2.0; 4.0; 2.0;], (([] : terminal list), [16.0; 2.0;]))
        TestCaseData([UnaryPlus;], [2.0; 2.0;], (([] : terminal list), [2.0; 2.0;]))
        TestCaseData([UnaryMinus;], [3.0; 2.0;], (([] : terminal list), [-3.0; 2.0;]))
    ]
    
[<TestCaseSource("ValidPerformOperationCases")>]
let GivenPerformOperation_WhenPassedValidInput_ReturnCorrectTuple(opList: terminal list, numList: float list, res: terminal list * float list) =
    let result = performOperation opList numList
    Assert.That(result, Is.EqualTo(res))
    
let InvalidPerformOperationsCases =
    [
        TestCaseData(([] : terminal list), ([] : float list))
        TestCaseData(([] : terminal list), [1.0;])
        TestCaseData(([] : terminal list), [1.0; 2.0;])
        TestCaseData([Rpar;], ([] : float list))
        TestCaseData([Rpar], [1.0;])
        TestCaseData([Rpar], [1.0; 2.0;])
        TestCaseData([Lpar;], ([] : float list))
        TestCaseData([Lpar], [1.0;])
        TestCaseData([Lpar], [1.0; 2.0;])
        TestCaseData([Plus;], ([] : float list))
        TestCaseData([Plus;], [1.0;])
        TestCaseData([Minus;], ([] : float list))
        TestCaseData([Minus], [1.0;])
        TestCaseData([Times;], ([] : float list))
        TestCaseData([Times;], [1.0;])
        TestCaseData([Divide;], ([] : float list))
        TestCaseData([Divide], [1.0;])
        TestCaseData([Exponent;], ([] : float list))
        TestCaseData([Exponent;], [1.0;])
        TestCaseData([UnaryPlus;], ([] : float list))
        TestCaseData([UnaryMinus], ([] : float list))
    ]
    
[<TestCaseSource("InvalidPerformOperationsCases")>]
let GivenPerformOperations_WhenPassedIncompleteArguments_RaiseExecError(opList: terminal list, numList: float list) =
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
        TestCaseData([Lpar;], [1.0; 2.0;], (([] : terminal list), [1.0; 2.0;]))
        TestCaseData([Plus; Lpar;], [1.0; 2.0;], (([] : terminal list), [3.0;]))
        TestCaseData([Divide; Times; Plus; Lpar;], [1.0; 2.0; 4.0; 4.0;], (([] : terminal list), [12.0;]))
        TestCaseData([Lpar; Plus;], [1.0; 2.0;], ([Plus;], [1.0; 2.0;]))
        TestCaseData([Plus; Lpar; Minus], [1.0; 2.0;], ([Minus;], [3.0;]))
        TestCaseData([Divide; Times; Plus; Lpar; Plus; Times; Divide;], [1.0; 2.0; 4.0; 4.0;], ([Plus; Times; Divide;], [12.0;]))
    ]
    
[<TestCaseSource("ValidEvaluateBracketsCases")>]
let GivenEvaluateBrackets_WhenPassedValidBracketedExpression_ThenReturnCorrectTuple(opList: terminal list, numList: float list, outLists: terminal list * float list) =
    let result = evaluateBrackets opList numList
    Assert.That(result, Is.EqualTo(outLists))

let InvalidEvaluateBracketsCases =
    [
        TestCaseData(([] : terminal list), ([] : float list))
        TestCaseData(([] : terminal list), [1.0;])
        TestCaseData(([] : terminal list), [1.0; 2.0;])
        TestCaseData([Rpar;], ([] : float list))
        TestCaseData([Rpar;], [1.0;])
        TestCaseData([Rpar;], [1.0; 2.0;])
        TestCaseData([UnaryMinus; Lpar;], ([] : float list))
        TestCaseData([Plus; Lpar;], [1.0;])
        TestCaseData([Plus; Times; Divide; Lpar;], [1.0; 2.0; 4.0;])
        TestCaseData([Plus;], [1.0; 2.0;])
        TestCaseData([Divide; Times; Plus;], [1.0; 2.0; 4.0; 4.0;])
    ]
    
[<TestCaseSource("InvalidEvaluateBracketsCases")>]
let GivenEvaluateBrackets_WhenPassedInvalidExpression_RaiseExecError(opList: terminal list, numList: float list) =
    Assert.Throws<ExecError>(fun () -> evaluateBrackets opList numList |> ignore) |> ignore