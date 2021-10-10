module Lexer.Tests

open NUnit.Framework
open Lexer.Parser

[<TestFixture>]
type ParserTests () =
    
    [<Test>]
    member this.GivenScan_WhenPassedPlusSign_ThenRecordPlus() =
        let result = Parser.scan ["+";] []
        Assert.That(result, Is.EqualTo[Parser.terminal.Plus;])
        
    [<Test>]
    member this.GivenScan_WhenPassedTimesSign_ThenRecordTimes() =
        let result = Parser.scan ["*";] []
        Assert.That(result, Is.EqualTo[Parser.terminal.Times;])
        
    [<Test>]
    member this.GivenScan_WhenPassedLeftBracket_ThenRecordLpar() =
        let result = Parser.scan ["(";] []
        Assert.That(result, Is.EqualTo[Parser.terminal.Lpar;])
        
    [<Test>]
    member this.GivenScan_WhenPassedRightBracket_ThenRecordRpar() =
        let result = Parser.scan [")";] []
        Assert.That(result, Is.EqualTo[Parser.terminal.Rpar;])
        
    [<Test>]
    member this.GivenScan_WhenPassedNumber_ThenRecordIntNumber() =
        let result = Parser.scan ["123";] []
        Assert.That(result, Is.EqualTo[Parser.terminal.Int 123;])
        
    [<Test>]
    member this.GivenScan_WhenPassedTokensRepresentingValidExpression_ThenRecordEquivalentTerminalsInSameOrder() =
        let result = Parser.scan ["1"; "*"; "5"; "*"; "("; "5"; "+"; "6"; ")"] []
        Assert.That(result, Is.EqualTo[Parser.terminal.Int 1; Parser.terminal.Times; Parser.terminal.Int 5; Parser.terminal.Times; Parser.terminal.Lpar; Parser.terminal.Int 5; Parser.terminal.Plus; Parser.terminal.Int 6; Parser.terminal.Rpar;])
        
    [<Test>]
    member this.GivenScan_WhenPassedTokensRepresentingInvalidExpression_ThenRecordEquivalentTerminalsInSameOrder() =
        let result = Parser.scan ["1"; ")"; "5"; "*"; "("; "*"; "+"; "6"; ")"] []
        Assert.That(result, Is.EqualTo[Parser.terminal.Int 1; Parser.terminal.Rpar; Parser.terminal.Int 5; Parser.terminal.Times; Parser.terminal.Lpar; Parser.terminal.Times; Parser.terminal.Plus; Parser.terminal.Int 6; Parser.terminal.Rpar;])

    [<Test>]
    member this.GivenScan_WhenPassedInvalidToken_ThenRaiseScanerror() =
        Assert.Throws<Parser.Scanerror>(fun () -> Parser.scan ["1a23";] [] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedInvalidExpression_RaiseParseerror() =
        Assert.Throws<Parser.Parseerror>(fun () -> Parser.expression [Parser.terminal.Times; Parser.terminal.Plus;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidAddition_ReturnEmptyArray() =
        let result = Parser.expression [Parser.terminal.Int 1; Parser.terminal.Plus; Parser.terminal.Int 5;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidMultiplication_ReturnEmptyArray() =
        let result = Parser.expression [Parser.terminal.Int 1; Parser.terminal.Times; Parser.terminal.Int 5;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidNestedExpression_ReturnEmptyArray() =
        let result = Parser.expression [Parser.terminal.Int 1; Parser.terminal.Times; Parser.terminal.Int 5; Parser.terminal.Times; Parser.terminal.Lpar; Parser.terminal.Int 5; Parser.terminal.Plus; Parser.terminal.Int 6; Parser.terminal.Rpar;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedLeftBracketWithoutRight_RaiseParseError() =
        Assert.Throws<Parser.Parseerror>(fun () -> Parser.expression [Parser.terminal.Lpar; Parser.terminal.Int 1; Parser.terminal.Plus; Parser.terminal.Int 1] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedRightBracketWithoutLeft_RaiseParseError() =
        Assert.Throws<Parser.Parseerror>(fun () -> Parser.expression [Parser.terminal.Lpar; Parser.terminal.Int 1; Parser.terminal.Plus; Parser.terminal.Int 1] |> ignore) |> ignore