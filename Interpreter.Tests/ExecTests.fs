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
        TestCaseData(-10, -10, -10.0**(-10.0))
        TestCaseData(7, 2, 49.0)
        TestCaseData(-7, 2, 49.0)
    ]
    
[<TestCaseSource("CalculatePlusData")>]
let GivenCalculate_WhenPassedSimpleAddition_ReturnCorrectAnswer(op1: float, op2: float , res: float) =
    let result = calculate Plus (Float op1) (Float op2)
    Assert.That(result, Is.EqualTo(float res))
    
[<TestCaseSource("CalculateMinusData")>]
let GivenCalculate_WhenPassedSimpleSubtraction_ReturnCorrectAnswer(op1: float, op2: float , res: float) =
    let result = calculate Minus (Float op1) (Float op2)
    Assert.That(result, Is.EqualTo(float res))
    
[<TestCaseSource("CalculateTimesData")>]
let GivenCalculate_WhenPassedSimpleMultiplication_ReturnCorrectAnswer(op1: float, op2: float , res: float) =
    let result = calculate Times (Float op1) (Float op2)
    Assert.That(result, Is.EqualTo(float res))
    
[<TestCaseSource("CalculateDivideData")>]
let GivenCalculate_WhenPassedSimpleDivision_ReturnCorrectAnswer(op1: float, op2: float , res: float) =
    let result = calculate Divide (Float op1) (Float op2)
    Assert.That(result, Is.EqualTo(float res))
    
[<TestCaseSource("CalculateExponentData")>]
let GivenCalculate_WhenPassedSimpleExponent_ReturnCorrectAnswer(op1: float, op2: float , res: float) =
    let result = calculate Exponent (Float op1) (Float op2)
    Assert.That(result, Is.EqualTo(float res))
        