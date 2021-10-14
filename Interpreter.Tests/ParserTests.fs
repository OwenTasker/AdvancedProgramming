module Lexer.Tests.ParserTests

open NUnit.Framework
open Interpreter.Parser
open Interpreter.Util

[<TestFixture>]
type ParserTests () =
        
    [<Test>]
    member this.GivenExpression_WhenPassedInvalidExpression_RaiseParseerror() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Times; terminal.Plus;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidAddition_ReturnEmptyArray() =
        let result = expression [terminal.Float 1.0; terminal.Plus; terminal.Float 5.0;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedAdditionWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Plus; terminal.Float 5.0;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedAdditionWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Float 1.0; terminal.Plus;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidSubtraction_ReturnEmptyArray() =
        let result = expression [terminal.Float 1.0; terminal.Minus; terminal.Float 5.0;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedSubtractionWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Minus; terminal.Float 5.0;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedSubtractionWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Float 1.0; terminal.Minus;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidMultiplication_ReturnEmptyArray() =
        let result = expression [terminal.Float 1.0; terminal.Times; terminal.Float 5.0;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedMultiplicationWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Times; terminal.Float 5.0;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedMultiplicationWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Float 1.0; terminal.Times;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidDivision_ReturnEmptyArray() =
        let result = expression [terminal.Float 1.0; terminal.Divide; terminal.Float 5.0;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedDivisionWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Divide; terminal.Float 5.0;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedDivisionWithNoSucceedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Float 1.0; terminal.Divide;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidNestedExpression_ReturnEmptyArray() =
        let result = expression [terminal.Float 1.0; terminal.Times; terminal.Float 5.0; terminal.Times; terminal.Lpar; terminal.Float 5.0; terminal.Plus; terminal.Float 6.0; terminal.Rpar;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedLeftBracketWithoutRight_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Lpar; terminal.Float 1.0; terminal.Plus; terminal.Float 1.0] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedRightBracketWithoutLeft_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Lpar; terminal.Float 1.0; terminal.Plus; terminal.Float 1.0] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_IntegerFollowedByValidBracketedExpression_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Float 5.0; terminal.Lpar; terminal.Float 5.0; terminal.Plus; terminal.Float 6.0; terminal.Rpar;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_ValidBracketedExpressionFollowedByInteger_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Lpar; terminal.Float 5.0; terminal.Plus; terminal.Float 6.0; terminal.Rpar; terminal.Float 5.0] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedIntegerWithExponentAndInteger_ReturnEmptyArray() =
        let result = expression [terminal.Float 2.0; terminal.Exponent; terminal.Float 3.0;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedIntegerWithExponentSignOnly_RaiseParseerror() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Float 2.0; terminal.Exponent] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedExpressionWithNestedExponents_ReturnEmptyArray() =
        let result = expression [terminal.Float 1.0; terminal.Exponent; terminal.Float 5.0; terminal.Times; terminal.Lpar; terminal.Float 5.0; terminal.Plus; terminal.Float 6.0; terminal.Exponent; terminal.Float 2.0; terminal.Rpar; terminal.Exponent; terminal.Float 2.0; terminal.Exponent; terminal.Lpar; terminal.Float 1.0; terminal.Rpar;]
        Assert.That(result, Is.EqualTo([]))
    
    [<Test>]
    member this.GivenExpression_WhenPassedExponentWithNoPrecedingValue_RaiseParseError() =
        Assert.Throws<Parseerror>(fun () -> expression [terminal.Exponent; terminal.Float 5.0;] |> ignore) |> ignore