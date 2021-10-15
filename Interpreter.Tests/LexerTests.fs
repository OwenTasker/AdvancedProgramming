module Interpreter.Tests.LexerTests


open NUnit.Framework
open Interpreter.Lexer
open Interpreter.Util


[<TestFixture>]
type LexerTests ()=        
 
    //Tokenizer Testing ---------------------------------------------------------
    [<Test>]
    member this.GivenTokenize_WhenPassedEmptyExpression_ThenReturnEmptyList() =
        let result = tokenize[""]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfSpace_ThenReturnEmptyList() =
        let result = tokenize[" "]
        Assert.That(result, Is.EqualTo([]))
    
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
    member this.GivenTokenize_WhenPassedExpressionConsistingOfMultipleAlphabeticalCharacters_ThenReturnCorrectTokens() =
        let result = tokenize["T";"h";"i";"s"]
        Assert.That(result, Is.EqualTo(["This"]))
    
    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfMultipleAlphabeticalCharactersAndDigits_ThenReturnCorrectTokens() =
        let result = tokenize["T";"h";"i";"s";"5";"T";"h";"a";"t"]
        Assert.That(result, Is.EqualTo(["This";"5";"That"]))
        
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
    
    //Scan function Testing -----------------------------------------------------
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
    member this.GivenScan_WhenPassedFunction_ThenRecordFunction() =
        let result = scan ["sqrt";] []
        Assert.That(result, Is.EqualTo[terminal.Function "sqrt";])

    [<Test>]
    member this.GivenScan_WhenPassedMultipleFunctions_ThenRecordFunctionS() =
        let result = scan ["sqrt";"ceil";] []
        Assert.That(result, Is.EqualTo[terminal.Function "sqrt"; terminal.Function "ceil"])
    
    
    [<Test>]
    member this.GivenScan_WhenPassedInvalidNumber_ThenRaiseFormatException() =
        Assert.Throws<System.FormatException>(fun () -> scan ["1a23";] [] |> ignore) |> ignore
    
    [<Test>]
    member this.GivenScan_WhenPassedInvalidLetter_ThenRaiseFormatException() =
        Assert.Throws<System.FormatException>(fun () -> scan ["a1a23";] [] |> ignore) |> ignore
    
    [<Test>]
    member this.GivenScan_WhenPassedInvalidToken_ThenRaiseScanError() =
        Assert.Throws<ScanError>(fun () -> scan ["?";] [] |> ignore) |> ignore

    
    //Lexer Testing -----------------------------------------------------------
    [<Test>]
    member this.GivenLexer_WhenPassedEmptyExpression_ReturnTupleOfEmptyLists() =
        let result = lexer [""]
        Assert.That(result, Is.EqualTo(([],[])))
        
    [<Test>]
    member this.GivenLexer_WhenPassedValidExpression_ReturnCorrectTuple() =
        let result = lexer ["1";"0";"+";"1"]
        Assert.That(result, Is.EqualTo((["10";"+";"1"],[Float 10.0; Plus; Float 1.0])))
        
    [<Test>]
    member this.GivenLexer_WhenPassedInvalidExpression_ThrowTokenizeError() =
        Assert.Throws<TokenizeError>(fun _ -> lexer["?"] |> ignore) |> ignore
       