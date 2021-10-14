module Interpreter.Tests.LexerTests.TokenizerTests

open NUnit.Framework
open Interpreter.Util
open Interpreter.Lexer

[<TestFixture>]
type LexerTests ()=
    
    //Expect Empty Lists
    [<Test>]
    member this.GivenTokenize_WhenPassedEmptyExpression_ThenReturnEmptyList() =
        let result = tokenize[""]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfSpace_ThenReturnEmptyList() =
        let result = tokenize[" "]
        Assert.That(result, Is.EqualTo([]))
    
    //Single Character tests
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfSingleNumber_ThenReturnCorrectTokens() =
        let result = tokenize["1"]
        Assert.That(result, Is.EqualTo(["1"]))
        
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfSingleLowerCaseLetter_ThenReturnCorrectTokens() =
        let result = tokenize["a"]
        Assert.That(result, Is.EqualTo(["a"]))

    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfSingleUpperCaseLetter_ThenReturnCorrectTokens() =
        let result = tokenize["A"]
        Assert.That(result, Is.EqualTo(["A"]))
        
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfSinglePlusSymbol_ThenReturnCorrectTokens() =
        let result = tokenize["+"]
        Assert.That(result, Is.EqualTo(["+"]))

    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfSingleMinusSymbol_ThenReturnCorrectTokens() =
        let result = tokenize["-"]
        Assert.That(result, Is.EqualTo(["-"]))
   
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfSinglePowerSymbol_ThenReturnCorrectTokens() =
        let result = tokenize["^"]
        Assert.That(result, Is.EqualTo(["^"]))
        
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfSingleMultiplySymbol_ThenReturnCorrectTokens() =
        let result = tokenize["*"]
        Assert.That(result, Is.EqualTo(["*"]))
    
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfLeftParenthesis_ThenReturnCorrectTokens() =
        let result = tokenize["("]
        Assert.That(result, Is.EqualTo(["("]))
        
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfRightParenthesis_ThenReturnCorrectTokens() =
        let result = tokenize[")"]
        Assert.That(result, Is.EqualTo([")"]))
        
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfQuestionMark_ThenFailWithTokenizeError() =
        Assert.Throws<TokenizeError>(fun () -> tokenize["?"] |> ignore) |> ignore
    
    [<Test>]
    member this.GivenTokenize_WhenPassedAllExpressionsInARow() =
        let result = tokenize["+";"-";"^";"*";"(";")";"/";"="]
        Assert.That(result, Is.EqualTo(["+";"-";"^";"*";"(";")";"/";"="]))
    
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionEndingInSingleCharacterInt_ThenReturnCorrectTokens() =
        let result = tokenize["1"; "1"; "+"; "2";]
        Assert.That(result, Is.EqualTo(["11"; "+"; "2";]))
        
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionStartingInSingleCharacterInt_ThenReturnCorrectTokens() =
        let result = tokenize["1"; "+"; "2"; "2";]
        Assert.That(result, Is.EqualTo(["1"; "+"; "22";]))       
        
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionWithTwoBackToBackSymbols_ThenReturnCorrectTokens() =
        let result = tokenize["1";"2";"3";"+";"-";"4";"5";"6"]
        Assert.That(result, Is.EqualTo(["123";"+";"-";"456"]))

    
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfJustMultipleIntegerCharacter_ThenReturnCorrectTokens() =
        let result = tokenize["1";"2";"3"]
        Assert.That(result, Is.EqualTo(["123"]))
    
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfNumberDecimalPointNumber_ThenReturnCorrectTokens() =
        let result = tokenize["1";".";"5"]
        Assert.That(result, Is.EqualTo(["1.5"]))
        
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfNumberDecimalPointNumberPlusNumber_ThenReturnCorrectTokens() =
        let result = tokenize["1";".";"5";"+";"5"]
        Assert.That(result, Is.EqualTo(["1.5";"+";"5"]))
    