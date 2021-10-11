module Lexer.Tests.ParserTests

open NUnit.Framework
open Interpreter
open Interpreter.Parser
open Interpreter.Util

[<TestFixture>]
type ParserTests () =
        
    [<Test>]
    member this.GivenExpression_WhenPassedInvalidExpression_RaiseParseerror() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Times; terminal.Plus;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidAddition_ReturnEmptyArray() =
        let result = expression [terminal.Int 1; terminal.Plus; terminal.Int 5;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedAdditionWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Plus; terminal.Int 5;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedAdditionWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Int 1; terminal.Plus;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidSubtraction_ReturnEmptyArray() =
        let result = expression [terminal.Int 1; terminal.Minus; terminal.Int 5;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedSubtractionWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Minus; terminal.Int 5;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedSubtractionWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Int 1; terminal.Minus;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidMultiplication_ReturnEmptyArray() =
        let result = expression [terminal.Int 1; terminal.Times; terminal.Int 5;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedMultiplicationWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Times; terminal.Int 5;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedMultiplicationWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Int 1; terminal.Times;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidDivision_ReturnEmptyArray() =
        let result = expression [terminal.Int 1; terminal.Divide; terminal.Int 5;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedDivisionWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Divide; terminal.Int 5;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedDivisionWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Int 1; terminal.Divide;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidNestedExpression_ReturnEmptyArray() =
        let result = expression [terminal.Int 1; terminal.Times; terminal.Int 5; terminal.Times; terminal.Lpar; terminal.Int 5; terminal.Plus; terminal.Int 6; terminal.Rpar;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedLeftBracketWithoutRight_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Lpar; terminal.Int 1; terminal.Plus; terminal.Int 1] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedRightBracketWithoutLeft_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Lpar; terminal.Int 1; terminal.Plus; terminal.Int 1] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_IntegerFollowedByValidBracketedExpression_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Int 5; terminal.Lpar; terminal.Int 5; terminal.Plus; terminal.Int 6; terminal.Rpar;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_ValidBracketedExpressionFollowedByInteger_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Lpar; terminal.Int 5; terminal.Plus; terminal.Int 6; terminal.Rpar; terminal.Int 5] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedIntegerWithExponentAndInteger_ReturnEmptyArray() =
        let result = expression [terminal.Int 2; terminal.Exponent; terminal.Int 3;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedIntegerWithExponentSignOnly_RaiseParseerror() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Int 2; terminal.Exponent] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedExpressionWithNestedExponents_ReturnEmptyArray() =
        let result = expression [terminal.Int 1; terminal.Exponent; terminal.Int 5; terminal.Times; terminal.Lpar; terminal.Int 5; terminal.Plus; terminal.Int 6; terminal.Exponent; terminal.Int 2; terminal.Rpar; terminal.Exponent; terminal.Int 2; terminal.Exponent; terminal.Lpar; terminal.Int 1; terminal.Rpar;]
        Assert.That(result, Is.EqualTo([]))
    
    [<Test>]
    member this.GivenExpression_WhenPassedExponentWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Exponent; terminal.Int 5;] |> ignore) |> ignore