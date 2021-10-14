module Lexer.Tests.ExecTests

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
let GivenCalculate_WhenPassedSimpleAddition_ReturnCorrectAnswer(op1: float, op2: float , res: float) =
    let result = calculate Plus op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("CalculateMinusData")>]
let GivenCalculate_WhenPassedSimpleSubtraction_ReturnCorrectAnswer(op1: float, op2: float , res: float) =
    let result = calculate Minus op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("CalculateTimesData")>]
let GivenCalculate_WhenPassedSimpleMultiplication_ReturnCorrectAnswer(op1: float, op2: float , res: float) =
    let result = calculate Times op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("CalculateDivideData")>]
let GivenCalculate_WhenPassedSimpleDivision_ReturnCorrectAnswer(op1: float, op2: float , res: float) =
    let result = calculate Divide op1 op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("CalculateExponentData")>]
let GivenCalculate_WhenPassedSimpleExponent_ReturnCorrectAnswer(op1: float, op2: float , res: float) =
    let result = calculate Exponent op1 op2
    Assert.That(result, Is.EqualTo(res))
    
let ReduceCases =
    [
        //SIMPLE ADDITION CASES
        TestCaseData([Float 1.0; Plus; Float 1.0], 2.0)
        TestCaseData([Float 1.0; Plus; Float 0.0], 1.0)
        TestCaseData([Float 0.0; Plus; Float 67.0], 67.0)
        TestCaseData([Float 0.0; Plus; Float 0.0], 0.0)
        TestCaseData([Float 5.0; Plus; Float -5.0], 0.0)
        TestCaseData([Float -5.0; Plus; Float -5.0], -10.0)
        TestCaseData([Float -5.0; Plus; Float 5.0], 0.0)
        TestCaseData([Float 10.0; Plus; Float -100.0], -90.0)
        //SIMPLE MULTIPLY CASES
        TestCaseData([Float 1.0; Times; Float 8.0], 8.0)
        TestCaseData([Float 8.0; Times; Float 1.0], 8.0)
        TestCaseData([Float 0.0; Times; Float 11.0], 0.0)
        TestCaseData([Float 10.0; Times; Float 0.0], 0.0)
        TestCaseData([Float 0.0; Times; Float 0.0], 0.0)
        TestCaseData([Float -10.0; Times; Float -10.0], 100.0)
        TestCaseData([Float 10.0; Times; Float 10.0], 100.0)
        TestCaseData([Float -10.0; Times; Float 10.0], -100.0)
        TestCaseData([Float 10.0; Times; Float -10.0], -100.0)
        //SIMPLE DIVIDE CASES
        TestCaseData([Float 1.0; Divide; Float 8.0], 0.125)
        TestCaseData([Float 8.0; Divide; Float 1.0], 8.0)
        TestCaseData([Float 0.0; Divide; Float 11.0], 0.0)
        TestCaseData([Float -10.0; Divide; Float -10.0], 1.0)
        TestCaseData([Float 10.0; Divide; Float 10.0], 1.0)
        TestCaseData([Float -10.0; Divide; Float 10.0], -1.0)
        TestCaseData([Float 10.0; Divide; Float -10.0], -1.0)
        //SIMPLE EXPONENT CASES
        TestCaseData([Float 1.0; Exponent; Float 8.0], 1.0)
        TestCaseData([Float 8.0; Exponent; Float 1.0], 8.0)
        TestCaseData([Float 0.0; Exponent; Float 11.0], 0.0)
        TestCaseData([Float 11.0; Exponent; Float 0.0], 1.0)
        TestCaseData([Float 0.0; Exponent; Float 0.0], 1.0)
        TestCaseData([Float -10.0; Exponent; Float -10.0], -10.0 ** -10.00)
        TestCaseData([Float 10.0; Exponent; Float 2.0], 100.0)
        TestCaseData([Float -10.0; Exponent; Float 2.0], 100.0)
        TestCaseData([Float 10.0; Exponent; Float 3.0], 1000.0)
        TestCaseData([Float -10.0; Exponent; Float 3.0], -1000.0)
        TestCaseData([Float 10.0; Exponent; Float -3.0], 0.001)
        TestCaseData([Float -10.0; Exponent; Float -3.0], -0.001)
        //ORDER OF OPERATIONS CASES
        TestCaseData([Float 1.0; Plus; Float 8.0; Minus; Float 6.0], 3.0)
        TestCaseData([Float 1.0; Minus; Float 8.0; Plus; Float 6.0], -1.0)
        TestCaseData([Float 64.0; Times; Float 8.0; Divide; Float 4.0], 128.0)
        TestCaseData([Float 64.0; Divide; Float 4.0; Times; Float 8.0], 128.0)
        TestCaseData([Float 7.0; Plus; Float 8.0; Times; Float 6.0], 55.0)
        TestCaseData([Float 9.0; Times; Float 8.0; Plus; Float 6.0], 78.0)
        TestCaseData([Float 6.0; Times; Float 8.0; Minus; Float 4.0], 44.0)
        TestCaseData([Float 6.0; Minus; Float 4.0; Times; Float 8.0], -26.0)
        TestCaseData([Float 1.0; Plus; Float 8.0; Divide; Float 4.0], 3.0)
        TestCaseData([Float 1.0; Divide; Float 8.0; Plus; Float 6.0], 6.125)
        TestCaseData([Float 64.0; Divide; Float 8.0; Minus; Float 4.0], 4.0)
        TestCaseData([Float 64.0; Minus; Float 4.0; Divide; Float 8.0], 63.5)
        TestCaseData([Float 1.0; Plus; Float 8.0; Exponent; Float 2.0], 65.0)
        TestCaseData([Float 1.0; Exponent; Float 8.0; Plus; Float 6.0], 7.0)
        TestCaseData([Float 2.0; Minus; Float 8.0; Exponent; Float 2.0], -62.0)
        TestCaseData([Float 16.0; Exponent; Float 2.0; Minus; Float 8.0], 248.0)
        TestCaseData([Float 7.0; Times; Float 5.0; Exponent; Float 3.0], 875.0)
        TestCaseData([Float 9.0; Exponent; Float 2.0; Times; Float 6.0], 486.0)
        TestCaseData([Float 64.0; Divide; Float 8.0; Exponent; Float 2.0], 1.0)
        TestCaseData([Float 6.0; Exponent; Float 3.0; Divide; Float 2.0], 108.0)
        TestCaseData([Lpar; Float 1.0; Plus; Float 8.0; Rpar; Minus; Float 6.0], 3.0)
        TestCaseData([Lpar; Float 1.0; Minus; Float 8.0; Rpar; Plus; Float 6.0], -1.0)
        TestCaseData([Lpar; Float 64.0; Times; Float 8.0; Rpar; Divide; Float 4.0], 128.0)
        TestCaseData([Lpar; Float 64.0; Divide; Float 4.0; Rpar; Times; Float 8.0], 128.0)
        TestCaseData([Lpar; Float 7.0; Plus; Float 8.0; Rpar; Times; Float 6.0], 90.0)
        TestCaseData([Lpar; Float 9.0; Times; Float 8.0; Rpar; Plus; Float 6.0], 78.0)
        TestCaseData([Lpar; Float 6.0; Times; Float 8.0; Rpar; Minus; Float 4.0], 44.0)
        TestCaseData([Lpar; Float 6.0; Minus; Float 4.0; Rpar; Times; Float 8.0], 16.0)
        TestCaseData([Lpar; Float 1.0; Plus; Float 8.0; Rpar; Divide; Float 4.0], 2.25)
        TestCaseData([Lpar; Float 1.0; Divide; Float 8.0; Rpar; Plus; Float 6.0], 6.125)
        TestCaseData([Lpar; Float 64.0; Divide; Float 8.0; Rpar; Minus; Float 4.0], 4.0)
        TestCaseData([Lpar; Float 64.0; Minus; Float 4.0; Rpar; Divide; Float 8.0], 7.5)
        TestCaseData([Lpar; Float 1.0; Plus; Float 8.0; Rpar; Exponent; Float 2.0], 81.0)
        TestCaseData([Lpar; Float 1.0; Exponent; Float 8.0; Rpar; Plus; Float 6.0], 7.0)
        TestCaseData([Lpar; Float 2.0; Minus; Float 8.0; Rpar; Exponent; Float 2.0], 36.0)
        TestCaseData([Lpar; Float 16.0; Exponent; Float 2.0; Rpar; Minus; Float 8.0], 248.0)
        TestCaseData([Lpar; Float 7.0; Times; Float 5.0; Rpar; Exponent; Float 3.0], 42875.0)
        TestCaseData([Lpar; Float 9.0; Exponent; Float 2.0; Rpar; Times; Float 6.0], 486.0)
        TestCaseData([Lpar; Float 64.0; Divide; Float 8.0; Rpar; Exponent; Float 2.0], 64.0)
        TestCaseData([Lpar; Float 6.0; Exponent; Float 3.0; Rpar; Divide; Float 2.0], 108.0)
    ]
    
[<TestCaseSource("ReduceCases")>]
let GivenReduce_WhenPassedTokens_ReturnCorrectAnswer(tokens: terminal list, expected: float) =
    let result = reduce tokens
    Assert.That(result, Is.EqualTo(expected))
        