﻿module Interpreter.Tests.LexerTests.ScannerTests

open NUnit.Framework
open Interpreter.Lexer
open Interpreter.Util


[<TestFixture>]
type ScannerTests ()=
    
    //Scan function Testing
    [<Test>]
    member this.GivenScan_WhenPassedPlusSign_ThenRecordPlus() =
        let result = scan ["+";] []
        Assert.That(result, Is.EqualTo[terminal.Plus;])

    [<Test>]
    member this.GivenScan_WhenPassedMinusSign_ThenRecordMinus() =
        let result = scan ["-";] []
        Assert.That(result, Is.EqualTo[terminal.Minus;])
        
    [<Test>]
    member this.GivenScan_WhenPassedExponentSign_ThenRecordExponent() =
        let result = scan ["^";] []
        Assert.That(result, Is.EqualTo[terminal.Exponent;])
        
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
    member this.GivenScan_WhenPassedDivideSign_ThenRecordDivide() =
        let result = scan ["/";] []
        Assert.That(result, Is.EqualTo[terminal.Divide;])
        
    [<Test>]
    member this.GivenScan_WhenPassedEqualsSign_ThenRecordEquals() =
        let result = scan ["=";] []
        Assert.That(result, Is.EqualTo[terminal.Equals;])
        
    [<Test>]
    member this.GivenScan_WhenPassedNumber_ThenRecordFloatNumber() =
        let result = scan ["123";] []
        Assert.That(result, Is.EqualTo[terminal.Float 123.0;])
        
    [<Test>]
    member this.GivenScan_WhenPassedTwoNumbersInARow_ThenRecordTwoFloats() =
        let result = scan ["123";"123";] []
        Assert.That(result, Is.EqualTo[terminal.Float 123.0; terminal.Float 123.0])
        
    [<Test>]
    member this.GivenScan_WhenPassedTokensRepresentingValidExpression_ThenRecordEquivalentTerminalsInSameOrder() =
        let result = scan ["1"; "*"; "5"; "*"; "("; "5"; "+"; "6"; ")"] []
        Assert.That(result, Is.EqualTo[terminal.Float 1.0; terminal.Times; terminal.Float 5.0; terminal.Times; terminal.Lpar; terminal.Float 5.0; terminal.Plus; terminal.Float 6.0; terminal.Rpar;])
        
    [<Test>]
    member this.GivenScan_WhenPassedTokensRepresentingInvalidExpression_ThenRecordEquivalentTerminalsInSameOrder() =
        let result = scan ["1"; ")"; "5"; "*"; "("; "*"; "+"; "6"; ")"] []
        Assert.That(result, Is.EqualTo[terminal.Float 1.0; terminal.Rpar; terminal.Float 5.0; terminal.Times; terminal.Lpar; terminal.Times; terminal.Plus; terminal.Float 6.0; terminal.Rpar;])

    [<Test>]
    member this.GivenScan_WhenPassedSingleWord_ThenRecordSingleWord() =
        let result = scan ["abc";] []
        Assert.That(result, Is.EqualTo[terminal.Word "abc"])
        
    [<Test>]
    member this.GivenScan_WhenPassedTwoWordsInARow_ThenRecordTwoWords() =
        let result = scan ["abc";"abc";] []
        Assert.That(result, Is.EqualTo[terminal.Word "abc"; terminal.Word "abc"])
    
    [<Test>]
    member this.GivenScan_WhenPassedInvalidNumber_ThenRaiseFormatException() =
        Assert.Throws<System.FormatException>(fun () -> scan ["1a23";] [] |> ignore) |> ignore
    
    [<Test>]
    member this.GivenScan_WhenPassedInvalidLetter_ThenRaiseFormatException() =
        Assert.Throws<System.FormatException>(fun () -> scan ["a1a23";] [] |> ignore) |> ignore
    
    [<Test>]
    member this.GivenScan_WhenPassedInvalidToken_ThenRaiseScanError() =
        Assert.Throws<ScanError>(fun () -> scan ["?";] [] |> ignore) |> ignore
        
    