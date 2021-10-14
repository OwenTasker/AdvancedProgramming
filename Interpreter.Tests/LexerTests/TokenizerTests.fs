module Interpreter.Tests.LexerTests.TokenizerTests

open NUnit.Framework
open Interpreter.Lexer
open Interpreter.Util


[<TestFixture>]
type LexerTests ()=
         
    //Tokenizer function testing
    [<Test>]
    member this.GivenLex_WhenPassedExpressionEndingInSingleCharacterInt_ThenReturnCorrectTokens() =
        let result = tokenize["1"; "1"; "+"; "2";]
        Assert.That(result, Is.EqualTo(["11"; "+"; "2";]))
        
    [<Test>]
    member this.GivenLex_WhenPassedExpressionStartingInSingleCharacterInt_ThenReturnCorrectTokens() =
        let result = tokenize["1"; "+"; "2"; "2";]
        Assert.That(result, Is.EqualTo(["1"; "+"; "22";]))
        
    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfSingleNumber_ThenReturnCorrectTokens() =
        let result = tokenize["1"]
        Assert.That(result, Is.EqualTo(["1"]))

    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfSinglePlusSymbol_ThenReturnCorrectTokens() =
        let result = tokenize["+"]
        Assert.That(result, Is.EqualTo(["+"]))

    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfSingleMinusSymbol_ThenReturnCorrectTokens() =
        let result = tokenize["-"]
        Assert.That(result, Is.EqualTo(["-"]))
        
    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfSingleMultiplySymbol_ThenReturnCorrectTokens() =
        let result = tokenize["*"]
        Assert.That(result, Is.EqualTo(["*"]))

    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfSinglePowerSymbol_ThenReturnCorrectTokens() =
        let result = tokenize["^"]
        Assert.That(result, Is.EqualTo(["^"]))

    [<Test>]
    member this.GivenLex_WhenPassedEmptyExpression_ThenReturnEmptyList() =
        let result = tokenize[""]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenLex_WhenPassedExpressionWithTwoBackToBackSymbols_ThenReturnCorrectTokens() =
        let result = tokenize["1";"2";"3";"+";"-";"4";"5";"6"]
        Assert.That(result, Is.EqualTo(["123";"+";"-";"456"]))

    
    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfJustMultipleIntegerCharacter_ThenReturnCorrectTokens() =
        let result = tokenize["1";"2";"3"]
        Assert.That(result, Is.EqualTo(["123"]))
    
    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfNumberDecimalPointNumber_ThenReturnCorrectTokens() =
        let result = tokenize["1";".";"5"]
        Assert.That(result, Is.EqualTo(["1.5"]))
        
    [<Test>]
    member this.GivenLex_WhenPassedExpressionConsistingOfNumberDecimalPointNumberPlusNumber_ThenReturnCorrectTokens() =
        let result = tokenize["1";".";"5";"+";"5"]
        Assert.That(result, Is.EqualTo(["1.5";"+";"5"]))
    