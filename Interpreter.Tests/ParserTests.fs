module Interpreter.Tests.ParserTests

open NUnit.Framework
open Interpreter.Parser
open Interpreter.Util

[<TestFixture>]
type ParserTests () =
        
    [<Test>]
    member this.GivenExpression_WhenPassedInvalidExpression_RaiseParseerror() =
        Assert.Throws<ParseError>(fun () -> expression [Times; Plus;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidAddition_ReturnEmptyArray() =
        let result = expression [Number 1.0; Plus; Number 5.0;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedAdditionWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Plus; Number 5.0;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedAdditionWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Number 1.0; Plus;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidSubtraction_ReturnEmptyArray() =
        let result = expression [Number 1.0; Minus; Number 5.0;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedSubtractionWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Minus; Number 5.0;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedSubtractionWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Number 1.0; Minus;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidMultiplication_ReturnEmptyArray() =
        let result = expression [Number 1.0; Times; Number 5.0;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedMultiplicationWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Times; Number 5.0;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedMultiplicationWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Number 1.0; Times;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidDivision_ReturnEmptyArray() =
        let result = expression [Number 1.0; Divide; Number 5.0;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedDivisionWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Divide; Number 5.0;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedDivisionWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Number 1.0; Divide;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidNestedExpression_ReturnEmptyArray() =
        let result = expression [Number 1.0; Times; Number 5.0; Times; Lpar; Number 5.0; Plus; Number 6.0; Rpar;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedLeftBracketWithoutRight_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Lpar; Number 1.0; Plus; Number 1.0] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedRightBracketWithoutLeft_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Lpar; Number 1.0; Plus; Number 1.0] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_IntegerFollowedByValidBracketedExpression_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Number 5.0; Lpar; Number 5.0; Plus; Number 6.0; Rpar;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_ValidBracketedExpressionFollowedByInteger_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Lpar; Number 5.0; Plus; Number 6.0; Rpar; Number 5.0] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedIntegerWithExponentAndInteger_ReturnEmptyArray() =
        let result = expression [Number 2.0; Exponent; Number 3.0;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedIntegerWithExponentSignOnly_RaiseParseerror() =
        Assert.Throws<ParseError>(fun () -> expression [Number 2.0; Exponent] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedExpressionWithNestedExponents_ReturnEmptyArray() =
        let result = expression [Number 1.0; Exponent; Number 5.0; Times; Lpar; Number 5.0; Plus; Number 6.0; Exponent; Number 2.0; Rpar; Exponent; Number 2.0; Exponent; Lpar; Number 1.0; Rpar;]
        Assert.That(result, Is.EqualTo([]))
    
    [<Test>]
    member this.GivenExpression_WhenPassedExponentWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<ParseError>(fun () -> expression [Exponent; Number 5.0;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedSimpleValidExpressionWithUnaryMinus_ReturnEmptyArray() =
        let result = expression [Number 2.0; Plus; UnaryMinus; Number 2.0]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedSimpleValidExpressionWithUnaryPlus_ReturnEmptyArray() =
        let result = expression [Number 2.0; Plus; UnaryPlus; Number 2.0]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidExpressionWithChainedUnaries_ReturnEmptyArray() =
        let result = expression [Number 2.0; Plus; UnaryMinus; UnaryPlus; UnaryMinus; UnaryPlus; UnaryMinus; UnaryPlus; UnaryMinus; UnaryMinus; Number 2.0]
        Assert.That(result, Is.EqualTo([]))
        