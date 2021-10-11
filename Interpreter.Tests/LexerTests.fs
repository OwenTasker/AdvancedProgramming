module Lexer.Tests.LexerTests

open NUnit.Framework
open Interpreter


[<TestFixture>]
type LexerTests ()=

    [<Test>]
    member this.GivenScan_WhenPassedPlusSign_ThenRecordPlus() =
        
        let result = Lexer.scan ["+";] []
        Assert.That(result, Is.EqualTo[Util.terminal.Plus;])
        
    [<Test>]
    member this.GivenScan_WhenPassedTimesSign_ThenRecordTimes() =
        let result = Lexer.scan ["*";] []
        Assert.That(result, Is.EqualTo[Util.terminal.Times;])
        
    [<Test>]
    member this.GivenScan_WhenPassedLeftBracket_ThenRecordLpar() =
        let result = Lexer.scan ["(";] []
        Assert.That(result, Is.EqualTo[Util.terminal.Lpar;])
        
    [<Test>]
    member this.GivenScan_WhenPassedRightBracket_ThenRecordRpar() =
        let result = Lexer.scan [")";] []
        Assert.That(result, Is.EqualTo[Util.terminal.Rpar;])
        
    [<Test>]
    member this.GivenScan_WhenPassedNumber_ThenRecordIntNumber() =
        let result = Lexer.scan ["123";] []
        Assert.That(result, Is.EqualTo[Util.terminal.Int 123;])
        
    [<Test>]
    member this.GivenScan_WhenPassedTokensRepresentingValidExpression_ThenRecordEquivalentTerminalsInSameOrder() =
        let result = Lexer.scan ["1"; "*"; "5"; "*"; "("; "5"; "+"; "6"; ")"] []
        Assert.That(result, Is.EqualTo[Util.terminal.Int 1; Util.terminal.Times; Util.terminal.Int 5; Util.terminal.Times; Util.terminal.Lpar; Util.terminal.Int 5; Util.terminal.Plus; Util.terminal.Int 6; Util.terminal.Rpar;])
        
    [<Test>]
    member this.GivenScan_WhenPassedTokensRepresentingInvalidExpression_ThenRecordEquivalentTerminalsInSameOrder() =
        let result = Lexer.scan ["1"; ")"; "5"; "*"; "("; "*"; "+"; "6"; ")"] []
        Assert.That(result, Is.EqualTo[Util.terminal.Int 1; Util.terminal.Rpar; Util.terminal.Int 5; Util.terminal.Times; Util.terminal.Lpar; Util.terminal.Times; Util.terminal.Plus; Util.terminal.Int 6; Util.terminal.Rpar;])

    [<Test>]
    member this.GivenScan_WhenPassedInvalidToken_ThenRaiseScanerror() =
        Assert.Throws<Util.Scanerror>(fun () -> Lexer.scan ["1a23";] [] |> ignore) |> ignore