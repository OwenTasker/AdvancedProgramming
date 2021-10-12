module Tests.LexerTests

open NUnit.Framework
open Interpreter.Lexer
open Interpreter.Util


[<TestFixture>]
type LexerTests ()=

    [<Test>]
    member this.GivenScan_WhenPassedPlusSign_ThenRecordPlus() =
        
        let result = scan ["+";] []
        Assert.That(result, Is.EqualTo[terminal.Plus;])
        
    [<Test>]
    member this.GivenScan_WhenPassedTimesSign_ThenRecordTimes() =
        let result = scan ["*";] []
        Assert.That(result, Is.EqualTo[terminal.Times;])
        
    [<Test>]
    member this.GivenScan_WhenPassedLeftBracket_ThenRecordLpar() =
        let result = scan ["(";] []
        Assert.That(result, Is.EqualTo[terminal.Lpar;])
        
    [<Test>]
    member this.GivenScan_WhenPassedRightBracket_ThenRecordRpar() =
        let result = scan [")";] []
        Assert.That(result, Is.EqualTo[terminal.Rpar;])
        
    [<Test>]
    member this.GivenScan_WhenPassedNumber_ThenRecordIntNumber() =
        let result = scan ["123";] []
        Assert.That(result, Is.EqualTo[terminal.Int 123;])
        
    [<Test>]
    member this.GivenScan_WhenPassedTokensRepresentingValidExpression_ThenRecordEquivalentTerminalsInSameOrder() =
        let result = scan ["1"; "*"; "5"; "*"; "("; "5"; "+"; "6"; ")"] []
        Assert.That(result, Is.EqualTo[terminal.Int 1; terminal.Times; terminal.Int 5; terminal.Times; terminal.Lpar; terminal.Int 5; terminal.Plus; terminal.Int 6; terminal.Rpar;])
        
    [<Test>]
    member this.GivenScan_WhenPassedTokensRepresentingInvalidExpression_ThenRecordEquivalentTerminalsInSameOrder() =
        let result = scan ["1"; ")"; "5"; "*"; "("; "*"; "+"; "6"; ")"] []
        Assert.That(result, Is.EqualTo[terminal.Int 1; terminal.Rpar; terminal.Int 5; terminal.Times; terminal.Lpar; terminal.Times; terminal.Plus; terminal.Int 6; terminal.Rpar;])

    [<Test>]
    member this.GivenScan_WhenPassedInvalidToken_ThenRaiseScanerror() =
        Assert.Throws<Scanerror>(fun () -> scan ["1a23";] [] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenLex_WhenPassedExpressionEndingInSingleCharacterInt_ThenReturnCorrectTokens() =
        let result = lex ["1"; "+"; "2";]
        Assert.That(result, Is.EqualTo(["1"; "+"; "2";]))