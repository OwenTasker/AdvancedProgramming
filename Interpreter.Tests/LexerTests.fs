module Interpreter.Tests.LexerTests

open NUnit.Framework
open Interpreter.Lexer
open Interpreter.Util


let TokenizeTestData =
    [
        TestCaseData([""], [])
        TestCaseData([" "], [])
        
        TestCaseData(["1"], ["1"])
        TestCaseData(["2"], ["2"])
        TestCaseData(["3"], ["3"])
        TestCaseData(["4"], ["4"])
        TestCaseData(["5"], ["5"])
        TestCaseData(["6"], ["6"])
        TestCaseData(["7"], ["7"])
        TestCaseData(["8"], ["8"])
        TestCaseData(["9"], ["9"])
        TestCaseData(["0"], ["0"])
        
        TestCaseData(["1";"2"], ["12"])
        TestCaseData(["1";"2";"3"], ["123"])
        TestCaseData(["1";"2";"3";"4"], ["1234"])
        TestCaseData(["1";"2";"3";"4";"5"], ["12345"])
        TestCaseData(["1";"2";"3";"4";"5";"6"], ["123456"])
        TestCaseData(["1";"2";"3";"4";"5";"6";"7"], ["1234567"])
        TestCaseData(["1";"2";"3";"4";"5";"6";"7";"8"], ["12345678"])
        TestCaseData(["1";"2";"3";"4";"5";"6";"7";"8";"9"], ["123456789"])
        TestCaseData(["1";"2";"3";"4";"5";"6";"7";"8";"9";"0"], ["1234567890"])
        
        TestCaseData(["+"], ["+"])
        TestCaseData(["*"], ["*"])
        TestCaseData(["/"], ["/"])
        TestCaseData(["-"], ["-"])
        TestCaseData(["^"], ["^"])
        TestCaseData(["("], ["("])
        TestCaseData([")"], [")"])
        TestCaseData(["="], ["="])
        
        TestCaseData(["+";"*"], ["+";"*"])
        TestCaseData(["+";"*";"/"], ["+";"*";"/"])
        TestCaseData(["+";"*";"/";"-"], ["+";"*";"/";"-";])
        TestCaseData(["+";"*";"/";"-";"^"], ["+";"*";"/";"-";"^"])
        TestCaseData(["+";"*";"/";"-";"^";"("], ["+";"*";"/";"-";"^";"("])
        TestCaseData(["+";"*";"/";"-";"^";"(";")"], ["+";"*";"/";"-";"^";"(";")"])
        TestCaseData(["+";"*";"/";"-";"^";"(";")";"="], ["+";"*";"/";"-";"^";"(";")";"="])
        
        TestCaseData(["a"], ["a"])
        TestCaseData(["A"], ["A"])
        TestCaseData(["a";"b"], ["ab"])
        TestCaseData(["A";"B"], ["AB"])
        TestCaseData(["a";"b";"c"], ["abc"])
        TestCaseData(["A";"B";"C"], ["ABC"])
        TestCaseData(["a";"b";"c";"d"], ["abcd"])
        TestCaseData(["A";"B";"C";"D"], ["ABCD"])
        TestCaseData(["a";"b";"c";"d";"e"], ["abcde"])
        TestCaseData(["A";"B";"C";"D";"E"], ["ABCDE"])
              
        
        TestCaseData(["1";"+";"1"], ["1";"+";"1"])
        TestCaseData(["1";"+";"1";"*";"1";"/";"1";"-";"1";"^";"1";"(";"1";")"],
                     ["1";"+";"1";"*";"1";"/";"1";"-";"1";"^";"1";"(";"1";")"])
        TestCaseData(["T";"h";"i";"s";" ";"i";"s";" ";"a";" ";"s";"e";"n";"t";"e";"n";"c";"e"],
                     ["This";"is";"a";"sentence"])
        TestCaseData(["1";".";"2";"5";"+";"1"], ["1.25"; "+"; "1"])
        TestCaseData(["1";".";"2";"5";"+";"1";"2";"5"], ["1.25"; "+"; "125"])
        TestCaseData([".";"2";"5";"+";"1";"2";"5"], [".25"; "+"; "125"])
        
    ]
    

[<TestCaseSource("TokenizeTestData")>]
let GivenTokenize_WhenPassedInput_ReturnCorrectTokens(op1, res) =
    let result = tokenize op1
    Assert.That(result, Is.EqualTo(res))    

type LexerTests ()=
    //Tokenizer Testing ---------------------------------------------------------

    [<Test>]
    member this.GivenTokenize_WhenPassedExpressionConsistingOfInvalidTokens_ThenFailWithTokenizeError() =
        Assert.Throws<TokenizeError>(fun () -> tokenize["?"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["_"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["["] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["]"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["{"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["}"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["£"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["$"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["@"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["<"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize[">"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["%"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["\""] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["!"] |> ignore) |> ignore
    

//    
//    //Scan function Testing -----------------------------------------------------
//    [<Test>]
//    member this.GivenScan_WhenPassedPlusSign_ThenRecordPlus() =
//        let result = scan ["+";] []
//        Assert.That(result, Is.EqualTo[terminal.Plus;])
//
//    [<Test>]
//    member this.GivenScan_WhenPassedMinusSign_ThenRecordMinus() =
//        let result = scan ["-";] []
//        Assert.That(result, Is.EqualTo[terminal.Minus;])
//        
//    [<Test>]
//    member this.GivenScan_WhenPassedExponentSign_ThenRecordExponent() =
//        let result = scan ["^";] []
//        Assert.That(result, Is.EqualTo[terminal.Exponent;])
//        
//    [<Test>]
//    member this.GivenScan_WhenPassedTimesSign_ThenRecordTimes() =
//        let result = scan ["*";] []
//        Assert.That(result, Is.EqualTo[terminal.Times;])
//        
//    [<Test>]
//    member this.GivenScan_WhenPassedLeftBracket_ThenRecordLpar() =
//        let result = scan ["(";] []
//        Assert.That(result, Is.EqualTo[terminal.Lpar;])
//        
//    [<Test>]
//    member this.GivenScan_WhenPassedRightBracket_ThenRecordRpar() =
//        let result = scan [")";] []
//        Assert.That(result, Is.EqualTo[terminal.Rpar;])
//        
//    [<Test>]
//    member this.GivenScan_WhenPassedDivideSign_ThenRecordDivide() =
//        let result = scan ["/";] []
//        Assert.That(result, Is.EqualTo[terminal.Divide;])
//        
//    [<Test>]
//    member this.GivenScan_WhenPassedEqualsSign_ThenRecordEquals() =
//        let result = scan ["=";] []
//        Assert.That(result, Is.EqualTo[terminal.Equals;])
//        
//    [<Test>]
//    member this.GivenScan_WhenPassedNumber_ThenRecordFloatNumber() =
//        let result = scan ["123";] []
//        Assert.That(result, Is.EqualTo[terminal.Number 123.0;])
//        
//    [<Test>]
//    member this.GivenScan_WhenPassedTwoNumbersInARow_ThenRecordTwoFloats() =
//        let result = scan ["123";"123";] []
//        Assert.That(result, Is.EqualTo[terminal.Number 123.0; terminal.Number 123.0])
//        
//    [<Test>]
//    member this.GivenScan_WhenPassedTokensRepresentingValidExpression_ThenRecordEquivalentTerminalsInSameOrder() =
//        let result = scan ["1"; "*"; "5"; "*"; "("; "5"; "+"; "6"; ")"] []
//        Assert.That(result, Is.EqualTo[terminal.Number 1.0; terminal.Times; terminal.Number 5.0; terminal.Times; terminal.Lpar; terminal.Number 5.0; terminal.Plus; terminal.Number 6.0; terminal.Rpar;])
//        
//    [<Test>]
//    member this.GivenScan_WhenPassedTokensRepresentingInvalidExpression_ThenRecordEquivalentTerminalsInSameOrder() =
//        let result = scan ["1"; ")"; "5"; "*"; "("; "*"; "+"; "6"; ")"] []
//        Assert.That(result, Is.EqualTo[terminal.Number 1.0; terminal.Rpar; terminal.Number 5.0; terminal.Times; terminal.Lpar; terminal.Times; terminal.Plus; terminal.Number 6.0; terminal.Rpar;])
//
//    [<Test>]
//    member this.GivenScan_WhenPassedSingleWord_ThenRecordSingleWord() =
//        let result = scan ["abc";] []
//        Assert.That(result, Is.EqualTo[terminal.Word "abc"])
//        
//    [<Test>]
//    member this.GivenScan_WhenPassedTwoWordsInARow_ThenRecordTwoWords() =
//        let result = scan ["abc";"abc";] []
//        Assert.That(result, Is.EqualTo[terminal.Word "abc"; terminal.Word "abc"])
//    
//    [<Test>]
//    member this.GivenScan_WhenPassedFunction_ThenRecordFunction() =
//        let result = scan ["sqrt";] []
//        Assert.That(result, Is.EqualTo[terminal.Function "sqrt";])
//
//    [<Test>]
//    member this.GivenScan_WhenPassedMultipleFunctions_ThenRecordFunctionS() =
//        let result = scan ["sqrt";"ceil";] []
//        Assert.That(result, Is.EqualTo[terminal.Function "sqrt"; terminal.Function "ceil"])
//    
//    
//    [<Test>]
//    member this.GivenScan_WhenPassedInvalidNumber_ThenRaiseFormatException() =
//        Assert.Throws<System.FormatException>(fun () -> scan ["1a23";] [] |> ignore) |> ignore
//    
//    [<Test>]
//    member this.GivenScan_WhenPassedInvalidLetter_ThenRaiseFormatException() =
//        Assert.Throws<System.FormatException>(fun () -> scan ["a1a23";] [] |> ignore) |> ignore
//    
//    [<Test>]
//    member this.GivenScan_WhenPassedInvalidToken_ThenRaiseScanError() =
//        Assert.Throws<ScanError>(fun () -> scan ["?";] [] |> ignore) |> ignore
//
//    
//    //Lexer Testing -----------------------------------------------------------
//    [<Test>]
//    member this.GivenLexer_WhenPassedEmptyExpression_ReturnTupleOfEmptyLists() =
//        let result = lexer [""]
//        Assert.That(result, Is.EqualTo(([],[])))
//        
//    [<Test>]
//    member this.GivenLexer_WhenPassedValidExpression_ReturnCorrectTuple() =
//        let result = lexer ["1";"0";"+";"1"]
//        Assert.That(result, Is.EqualTo((["10";"+";"1"],[Number 10.0; Plus; Number 1.0])))
//        
//    [<Test>]
//    member this.GivenLexer_WhenPassedInvalidExpression_ThrowTokenizeError() =
//        Assert.Throws<TokenizeError>(fun _ -> lexer["?"] |> ignore) |> ignore
//       