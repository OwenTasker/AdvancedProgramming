module Tests.LexerTests

open NUnit.Framework
open Interpreter.Lexer
open Interpreter.Util


[<TestFixture>]
type LexerTests ()=

    //Scan function Testing
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
         
    //Lex function testing
    [<Test>]
    member this.GivenLex_WhenPassedExpressionEndingInSingleCharacterInt_ThenReturnCorrectTokens() =
        let result = lex ["1"; "1"; "+"; "2";]
        Assert.That(result, Is.EqualTo(["11"; "+"; "2";]))
        
    [<Test>]
    member this.GivenLex_WhenPassedExpressionStartingInSingleCharacterInt_ThenReturnCorrectTokens() =
        let result = lex ["1"; "+"; "22";]
        Assert.That(result, Is.EqualTo(["1"; "+"; "22";]))
        
    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfSingleNumber_ThenReturnCorrectTokens() =
        let result = lex["1"]
        Assert.That(result, Is.EqualTo(["1"]))

    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfSinglePlusSymbol_ThenReturnCorrectTokens() =
        let result = lex["+"]
        Assert.That(result, Is.EqualTo(["+"]))

    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfSingleMinusSymbol_ThenReturnCorrectTokens() =
        let result = lex["-"]
        Assert.That(result, Is.EqualTo(["-"]))
        
    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfSingleMultiplySymbol_ThenReturnCorrectTokens() =
        let result = lex["*"]
        Assert.That(result, Is.EqualTo(["*"]))

    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfSinglePowerSymbol_ThenReturnCorrectTokens() =
        let result = lex["^"]
        Assert.That(result, Is.EqualTo(["^"]))

    [<Test>]
    member this.GivenLex_WhenPassedEmptyExpression_ThenReturnEmptyList() =
        let result = lex[""]
        Assert.That(result, Is.EqualTo([]))

    
    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfJustMultipleIntegerCharacter_ThenReturnCorrectTokens() =
        let result = lex["1";"2";"3"]
        Assert.That(result, Is.EqualTo(["123"]))
    
    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfNumberDecimalPointNumber_ThenReturnCorrectTokens() =
        let result = lex["1";".";"5"]
        Assert.That(result, Is.EqualTo(["1.5"]))
        
    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfNumberDecimalPointNumberPlusNumber() =
        let result = ["1";".";"5+5"]
        Assert.That(result, Is.EqualTo(["1.5"; "+"; "5"]))