module Interpreter.Tests.ExecTests

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
    
let UnaryData =
    [
        TestCaseData(UnaryMinus, 1, -1)
        TestCaseData(UnaryMinus, -1, 1)
        TestCaseData(UnaryPlus, 1, 1)
        TestCaseData(UnaryPlus, -1, -1)
    ]
    
[<TestCaseSource("UnaryData")>]
let GivenUnary_WhenPassedSimpleExpression_ReturnCorrectAnswer(op1: terminal, op2: float, res: float) =
    let result = unary op1 op2
    Assert.That(result, Is.EqualTo(res))
    
let ReduceCases =
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
    
[<TestCaseSource("ReduceCases")>]
let GivenReduce_WhenPassedTokens_ReturnCorrectAnswer(tokens: terminal list, expected: float) =
    let result = reduce tokens
    Assert.That(result, Is.EqualTo(expected))
        