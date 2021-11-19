module Interpreter.Tests.DifferentiateTests

open NUnit.Framework
open Interpreter.Differentiate
open Interpreter.Util

// Test that operators perform as expected

let PlusCases =
    [
        //Test const for equivalence relations
        //Test var and expr for building correct strings
        //Const*Const
        TestCaseData(Const(1.0, 0.0), Const(1.0, 0.0), Const(2.0, 0.0))
        TestCaseData(Const(1.0, 0.0), Const(0.0, 0.0), Const(1.0, 0.0))
        TestCaseData(Const(0.0, 0.0), Const(1.0, 0.0), Const(1.0, 0.0))
        TestCaseData(Const(0.0, 0.0), Const(0.0, 0.0), Const(0.0, 0.0))
        TestCaseData(Const(0.0, 0.0), Const(-4.0, 0.0), Const(-4.0, 0.0))
        TestCaseData(Const(-3.0, 0.0), Const(-4.0, 0.0), Const(-7.0, 0.0))
        //Const*Var
        TestCaseData(Const(-3.0, 0.0), Var(Word "x", 1.0), Expr([Number -3.0; Plus; Word "x"], [Number 1.0]))
        //Const*Expr
        TestCaseData(Const(-3.0, 0.0), Expr([Number -3.0; Plus; Word "x"], [Number 8.0]), Expr([Number -3.0; Plus; Number -3.0; Plus; Word "x"], [Number 8.0]))
        //Var*Const
        TestCaseData(Var(Word "x", 1.0), Const(-3.0, 0.0), Expr([Word "x"; Plus; Number -3.0 ], [Number 1.0]))
        //Var*Var
        TestCaseData(Var(Word "x", 1.0), Var(Word "x", 1.0), Expr([Word "x"; Plus; Word "x"], [Number 2.0]))
        //Var*Expr
        TestCaseData(Var(Word "x", 1.0), Expr([Number -3.0; Plus; Word "x"], [Number 8.0]), Expr([Word "x"; Plus; Number -3.0; Plus; Word "x"], [Number 1.0; Plus; Number 8.0]))
        //Expr*Const
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Const(-3.0, 0.0), Expr([Number -3.0; Plus; Word "x"; Plus; Number -3.0], [Number 1.0]))
        //Expr*Var
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Var(Word "x", 1.0), Expr([Number -3.0; Plus; Word "x"; Plus; Word "x"], [Number 1.0; Plus; Number 1.0]))
        //Expr*Expr
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Expr([Number -4.0; Plus; Word "x"], [Number 8.0]), Expr([Number -3.0; Plus; Word "x"; Plus; Number -4.0; Plus; Word "x"], [Number 1.0; Plus; Number 8.0]))
    ]
    
let MinusCases =
    [
        //Test const for equivalence relations
        //Test var and expr for building correct strings
        //Const*Const
        TestCaseData(Const(1.0, 0.0), Const(1.0, 0.0), Const(0.0, 0.0))
        TestCaseData(Const(1.0, 0.0), Const(0.0, 0.0), Const(1.0, 0.0))
        TestCaseData(Const(0.0, 0.0), Const(1.0, 0.0), Const(-1.0, 0.0))
        TestCaseData(Const(0.0, 0.0), Const(0.0, 0.0), Const(0.0, 0.0))
        TestCaseData(Const(0.0, 0.0), Const(-4.0, 0.0), Const(4.0, 0.0))
        TestCaseData(Const(-3.0, 0.0), Const(-4.0, 0.0), Const(1.0, 0.0))
        //Const*Var
        TestCaseData(Const(-3.0, 0.0), Var(Word "x", 1.0), Expr([Number -3.0; Minus; Word "x"], [Number -1.0]))
        //Const*Expr
        TestCaseData(Const(-3.0, 0.0), Expr([Number -3.0; Plus; Word "x"], [Number 8.0]), Expr([Number -3.0; Minus; Number -3.0; Plus; Word "x"], [UnaryMinus; Number 8.0]))
        //Var*Const
        TestCaseData(Var(Word "x", 1.0), Const(-3.0, 0.0), Expr([Word "x"; Minus; Number -3.0 ], [Number 1.0]))
        //Var*Var
        TestCaseData(Var(Word "x", 1.0), Var(Word "x", 1.0), Expr([Word "x"; Minus; Word "x"], [Number 0.0]))
        //Var*Expr
        TestCaseData(Var(Word "x", 1.0), Expr([Number -3.0; Plus; Word "x"], [Number 8.0]), Expr([Word "x"; Minus; Number -3.0; Plus; Word "x"], [Number 1.0; Minus; Number 8.0]))
        //Expr*Const
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Const(-3.0, 0.0), Expr([Number -3.0; Plus; Word "x"; Minus; Number -3.0], [Number 1.0]))
        //Expr*Var
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Var(Word "x", 1.0), Expr([Number -3.0; Plus; Word "x"; Minus; Word "x"], [Number 1.0; Minus; Number 1.0]))
        //Expr*Expr
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Expr([Number -4.0; Plus; Word "x"], [Number 8.0]), Expr([Number -3.0; Plus; Word "x"; Minus; Number -4.0; Plus; Word "x"], [Number 1.0; Minus; Number 8.0]))
    ]
    
let TimesCases =
    [
        //Test const for equivalence relations
        //Test var and expr for building correct strings
        //Const*Const
        TestCaseData(Const(1.0, 0.0), Const(1.0, 0.0), Const(1.0, 0.0))
        TestCaseData(Const(1.0, 0.0), Const(0.0, 0.0), Const(0.0, 0.0))
        TestCaseData(Const(0.0, 0.0), Const(1.0, 0.0), Const(0.0, 0.0))
        TestCaseData(Const(0.0, 0.0), Const(0.0, 0.0), Const(0.0, 0.0))
        TestCaseData(Const(2.0, 0.0), Const(-4.0, 0.0), Const(-8.0, 0.0))
        TestCaseData(Const(-3.0, 0.0), Const(-4.0, 0.0), Const(12.0, 0.0))
        //Const*Var
        TestCaseData(Const(-3.0, 0.0), Var(Word "x", 1.0), Expr([Number -3.0; Times; Word "x"], [Number -3.0]))
        //Const*Expr
        TestCaseData(Const(-3.0, 0.0), Expr([Number -3.0; Plus; Word "x"], [Number 8.0]), Expr([Number -3.0; Times; Lpar; Number -3.0; Plus; Word "x"; Rpar;], [Number -3.0; Times; Lpar; Number 8.0; Rpar]))
        //Var*Const
        TestCaseData(Var(Word "x", 1.0), Const(-3.0, 0.0), Expr([Word "x"; Times; Number -3.0 ], [Number -3.0]))
        //Var*Var
        TestCaseData(Var(Word "x", 1.0), Var(Word "x", 1.0), Expr([Word "x"; Times; Word "x"], [Word "x"; Plus; Word "x"]))
        //Var*Expr
        TestCaseData(Var(Word "x", 1.0), Expr([Number -3.0; Plus; Word "x"], [Number 8.0]), Expr([Word "x"; Times; Lpar; Number -3.0; Plus; Word "x"; Rpar;], [Word "x"; Times; Lpar; Number 8.0; Rpar; Plus; Number -3.0; Plus; Word "x"]))
        //Expr*Const
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Const(-3.0, 0.0), Expr([Lpar; Number -3.0; Plus; Word "x"; Rpar; Times; Number -3.0], [Lpar; Number 1.0; Rpar; Times; Number -3.0]))
        //Expr*Var
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Var(Word "x", 1.0), Expr([Lpar; Number -3.0; Plus; Word "x"; Rpar; Times; Word "x"], [Number -3.0; Plus; Word "x"; Plus; Lpar; Number 1.0; Rpar; Times; Word "x"]))
        //Expr*Expr
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Expr([Number -4.0; Plus; Word "x"], [Number 8.0]), Expr([Lpar; Number -3.0; Plus; Word "x"; Rpar; Times; Lpar; Number -4.0; Plus; Word "x"; Rpar], [Lpar; Number -3.0; Plus; Word "x"; Rpar; Times; Lpar; Number 8.0; Rpar; Plus; Lpar; Number 1.0; Rpar; Times; Lpar; Number -4.0; Plus; Word "x"; Rpar]))
    ]
    
let DivideCases =
    [
        //a/c + ((bc - ad)/c^2)e
        //Test const for equivalence relations
        //Test var and expr for building correct strings
        //Const*Const
        TestCaseData(Const(1.0, 0.0), Const(1.0, 0.0), Const(1.0, 0.0))
        TestCaseData(Const(0.0, 0.0), Const(1.0, 0.0), Const(0.0, 0.0))
        TestCaseData(Const(2.0, 0.0), Const(-4.0, 0.0), Const(-0.5, 0.0))
        TestCaseData(Const(-3.0, 0.0), Const(-3.0, 0.0), Const(1.0, 0.0))
        //Const*Var
        TestCaseData(Const(-3.0, 0.0), Var(Word "x", 1.0), Expr([Number -3.0; Divide; Word "x"], [UnaryMinus; Number -3.0; Divide; Word "x"; Exponent; Number 2.0]))
        //Const*Expr
        TestCaseData(Const(-3.0, 0.0), Expr([Number -3.0; Plus; Word "x"], [Number 8.0]), Expr([Number -3.0; Divide; Lpar; Number -3.0; Plus; Word "x"; Rpar], [Lpar; UnaryMinus; Number -3.0; Times; Lpar; Number 8.0; Rpar; Rpar; Divide; Lpar; Number -3.0; Plus; Word "x"; Rpar; Exponent; Number 2.0]))
        //Var*Const
        TestCaseData(Var(Word "x", 1.0), Const(-3.0, 0.0), Expr([Word "x"; Divide; Number -3.0 ], [Number (-3.0/9.0)]))
        //Var*Var
        TestCaseData(Var(Word "x", 1.0), Var(Word "x", 1.0), Expr([Number 1.0], [Number 0.0]))
        //Var*Expr
        TestCaseData(Var(Word "x", 1.0), Expr([Number -3.0; Plus; Word "x"], [Number 8.0]), Expr([Word "x"; Divide; Lpar; Number -3.0; Plus; Word "x"; Rpar;], [Lpar; Number -3.0; Plus; Word "x"; Minus; Word "x"; Times; Lpar; Number 8.0; Rpar; Rpar; Divide; Lpar; Number -3.0; Plus; Word "x"; Rpar; Exponent; Number 2.0]))
        //Expr*Const
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Const(-3.0, 0.0), Expr([Lpar; Number -3.0; Plus; Word "x"; Rpar; Divide; Number -3.0], [Lpar; Lpar; Number 1.0; Rpar; Times; Number -3.0; Rpar; Divide; Number -3.0; Exponent; Number 2.0]))
        //Expr*Var
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Var(Word "x", 1.0), Expr([Lpar; Number -3.0; Plus; Word "x"; Rpar; Divide; Word "x"], [Lpar; Lpar; Number 1.0; Rpar; Times; Word "x"; Minus; Lpar; Number -3.0; Plus; Word "x"; Rpar; Rpar; Divide; Word "x"; Exponent; Number 2.0]))
        //Expr*Expr
        TestCaseData(Expr([Number -3.0; Plus; Word "x"], [Number 1.0]), Expr([Number -4.0; Plus; Word "x"], [Number 8.0]), Expr([Lpar; Number -3.0; Plus; Word "x"; Rpar; Divide; Lpar; Number -4.0; Plus; Word "x"; Rpar;], [Lpar; Lpar; Number 1.0; Rpar; Times; Lpar; Number -4.0; Plus; Word "x"; Rpar; Minus; Lpar; Number -3.0; Plus; Word "x"; Rpar; Times; Lpar; Number 8.0; Rpar; Rpar; Divide; Lpar; Number -4.0; Plus; Word "x"; Rpar; Exponent; Number 2.0]))
    ]
    
let ExponentCases =
    [
        //(a + be)^(c + de) = (a^c) + (c*a^(c-1))e
        //Test const for equivalence relations
        //Test var and expr for building correct strings
        //Const*Const
        TestCaseData(Const(2.0, 0.0), Const(2.0, 0.0), Const(4.0, 0.0))
        TestCaseData(Const(1.0, 0.0), Const(0.0, 0.0), Const(1.0, 0.0))
        TestCaseData(Const(0.0, 0.0), Const(1.0, 0.0), Const(0.0, 0.0))
        TestCaseData(Const(0.0, 0.0), Const(0.0, 0.0), Const(1.0, 0.0))
        TestCaseData(Const(2.0, 0.0), Const(-1.0, 0.0), Const(0.5, 0.0))
        //Const*Var
        TestCaseData(Const(-3.0, 0.0), Var(Word "x", 1.0), Expr([Number -3.0; Exponent; Word "x"], [Function "ln"; Lpar; Number -3.0; Rpar; Times; Number -3.0; Exponent; Word "x"]))
        //Const*Expr
        TestCaseData(Const(-3.0, 0.0), Expr([Number -3.0; Plus; Word "x"], [Number 8.0]), Expr([Number -3.0; Exponent; Lpar; Number -3.0; Plus; Word "x"; Rpar], [Function "ln"; Lpar; Number -3.0; Rpar; Times; Lpar; Number 8.0; Rpar; Times; Number -3.0; Exponent; Lpar; Number -3.0; Plus; Word "x"; Rpar]))
        //Var*Const
        TestCaseData(Var(Word "x", 1.0), Const(-3.0, 0.0), Expr([Word "x"; Exponent; Number -3.0 ], [Number -3.0; Times; Word "x"; Exponent; Number -4.0]))
        //Var*Var
        TestCaseData(Var(Word "x", 1.0), Var(Word "x", 1.0), Expr([Word "x"; Exponent; Word "x"], [Word "x"; Exponent; Word "x"; Times; Lpar; Function "ln"; Lpar; Word "x"; Rpar; Plus; Number 1.0; Rpar]))
        //Var*Expr
        TestCaseData(Var(Word "x", 1.0), Expr([Number -3.0; Plus; Word "x"], [Number 8.0]), Expr([Word "x"; Exponent; Lpar; Number -3.0; Plus; Word "x"; Rpar;], [Word "x"; Exponent; Lpar; Number -3.0; Plus; Word "x"; Rpar; Times; Lpar; Lpar; Number 8.0; Rpar; Times; Function "ln"; Lpar; Word "x"; Rpar; Plus; Lpar; Number -3.0; Plus; Word "x"; Rpar; Divide; Word "x"; Rpar]))
        //Expr*Const
        TestCaseData(Expr([Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0], [Number 2.0; Times; Word "x"; Plus; Number 1.0]), Const(2.0, 0.0), Expr([Lpar; Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0; Rpar; Exponent; Number 2.0], [Number 2.0; Times; Lpar; Number 2.0; Times; Word "x"; Plus; Number 1.0; Rpar; Times; Lpar; Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0; Rpar; Exponent; Number 1.0;]))
        //Expr*Var
        TestCaseData(Expr([Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0], [Number 2.0; Times; Word "x"; Plus; Number 1.0]), Var(Word "x", 1.0), Expr([Lpar; Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0; Rpar; Exponent; Word "x"], [Lpar; Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0; Rpar; Exponent; Word "x"; Times; Lpar; Function "ln"; Lpar; Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0; Rpar; Plus; Lpar; Word "x"; Times; Lpar; Number 2.0; Times; Word "x"; Plus; Number 1.0; Rpar; Divide; Lpar; Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0; Rpar; Rpar;]))
        //Expr*Expr
        TestCaseData(Expr([Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0], [Number 2.0; Times; Word "x"; Plus; Number 1.0]), Expr([Number 2.0; Times; Word "x"; Exponent; Number 2.0; Plus; Number 1.0], [Number 4.0; Times; Word "x";]), Expr([Lpar; Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0; Rpar; Exponent; Lpar; Number 2.0; Times; Word "x"; Exponent; Number 2.0; Plus; Number 1.0; Rpar], [Lpar; Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0; Rpar; Exponent; Lpar; Number 2.0; Times; Word "x"; Exponent; Number 2.0; Plus; Number 1.0; Rpar; Times; Lpar; Lpar; Number 4.0; Times; Word "x"; Rpar; Times; Function "ln"; Lpar; Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0; Rpar; Plus; Lpar; Lpar; Number 2.0; Times; Word "x"; Plus; Number 1.0; Rpar; Times; Lpar; Number 2.0; Times; Word "x"; Exponent; Number 2.0; Plus; Number 1.0; Rpar; Rpar; Divide; Lpar; Word "x"; Exponent; Number 2.0; Plus; Word "x"; Plus; Number 1.0; Rpar; Rpar]))
    ]
    
[<TestCaseSource("PlusCases")>]
let GivenPerformBinaryOperation_WhenPassedSimpleAddition_ReturnCorrectAnswer(op1: Dual, op2: Dual, res: Dual) =
    let result = op1 + op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("MinusCases")>]
let GivenPerformBinaryOperation_WhenPassedSimpleSubtraction_ReturnCorrectAnswer(op1: Dual, op2: Dual, res: Dual) =
    let result = op1 - op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("TimesCases")>]
let GivenPerformBinaryOperation_WhenPassedSimpleMultiplication_ReturnCorrectAnswer(op1: Dual, op2: Dual, res: Dual) =
    let result = op1 * op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("DivideCases")>]
let GivenPerformBinaryOperation_WhenPassedSimpleDivision_ReturnCorrectAnswer(op1: Dual, op2: Dual, res: Dual) =
    let result = op1 / op2
    Assert.That(result, Is.EqualTo(res))
    
[<TestCaseSource("ExponentCases")>]
let GivenPerformBinaryOperation_WhenPassedSimpleExponentiation_ReturnCorrectAnswer(op1: Dual, op2: Dual, res: Dual) =
    let result = op1 ** op2
    Assert.That(result, Is.EqualTo(res))

// Test that perform binary operation calls the expected operator

// Test that perform binary operation throws errors
// Test that perform unary operation calls the expected operator
// Test that perform unary operation throws errors
// Test that perform operation calls the expected operator
// Test that perform operation throws errors
// Test that evaluate brackets updates stacks correctly
// Test that evaluate brackets throws errors
// Test that auto differentiate performs correct expected differentiation
// Test that auto differentiate throws errors
// Test that differentiate performs correct expected differentiation