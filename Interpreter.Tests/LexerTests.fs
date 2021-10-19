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
        
        TestCaseData(["+";"+"], ["+";"+"])
        TestCaseData(["*";"*"], ["*";"*"])
              
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
    
let ScannerTestData =
    [
        //Symbol Testing
        TestCaseData(["+"],[UnaryPlus])
        TestCaseData(["-"],[UnaryMinus])
        TestCaseData(["3";"+"],[Number 3.0;Plus])
        TestCaseData(["3";"-"],[Number 3.0;Minus])
        TestCaseData(["^"],[Exponent])
        TestCaseData(["*"],[Times])
        TestCaseData(["("],[Lpar])
        TestCaseData([")"],[Rpar])
        TestCaseData(["/"],[Divide])
        TestCaseData(["="],[Equals])
        
        //Unary Symbol Testing
        TestCaseData(["+";"+";"+"], [UnaryPlus;UnaryPlus;UnaryPlus])
        TestCaseData(["-";"-";"-"], [UnaryMinus;UnaryMinus;UnaryMinus])
        TestCaseData(["-";"(";"3";"+";"4";")";"-";"3"],
                     [UnaryMinus;Lpar;Number 3.0;Plus;Number 4.0;Rpar;Minus;Number 3.0])
        
        //Function Testing
        TestCaseData(["ceil"], [Function "ceil"])
        TestCaseData(["floor"], [Function "floor"])
        TestCaseData(["sqrt"], [Function "sqrt"])
        TestCaseData(["round"], [Function "round"])
        TestCaseData(["ceil"; "3.222"; "floor"; "3.222"],
                     [Function "ceil"; Number 3.222; Function "floor"; Number 3.222])
        
        //Word Testing
        TestCaseData(["a"], [Word "a"])
        TestCaseData(["b"], [Word "b"])
        TestCaseData(["c"], [Word "c"])
        TestCaseData(["d"], [Word "d"])
        TestCaseData(["e"], [Word "e"])
        TestCaseData(["A"], [Word "A"])
        TestCaseData(["B"], [Word "B"])
        TestCaseData(["C"], [Word "C"])
        TestCaseData(["D"], [Word "D"])
        TestCaseData(["E"], [Word "E"])
        TestCaseData(["Word";"Word"], [Word "Word"; Word "Word"])
        TestCaseData(["Word";"5";"Word"], [Word "Word"; Number 5.0; Word "Word"])
        
        //Number Testing
        TestCaseData(["1"], [Number 1.0])
        TestCaseData(["2"], [Number 2.0])
        TestCaseData(["3"], [Number 3.0])
        TestCaseData(["4"], [Number 4.0])
        TestCaseData(["5"], [Number 5.0])
        TestCaseData(["6"], [Number 6.0])
        TestCaseData(["7"], [Number 7.0])
        TestCaseData(["8"], [Number 8.0])
        TestCaseData(["9"], [Number 9.0])
        TestCaseData(["0"], [Number 0.0])
        TestCaseData([".2"], [Number 0.2])
        TestCaseData([".25"], [Number 0.25])
        TestCaseData([".252"], [Number 0.252])
    ]
    
[<TestCaseSource("ScannerTestData")>]
let GivenScan_WhenPassedInput_ReturnCorrectTokens(op1, res) =
    let result = scan op1 []
    Assert.That(result, Is.EqualTo(res))
    
    

type LexerErrorTests ()=
    //Tokenizer Error Testing ---------------------------------------------------------
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
        Assert.Throws<TokenizeError>(fun () -> tokenize["'"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["|"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["\\"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize[":"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize[";"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["#"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["~"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["¬"] |> ignore) |> ignore
        Assert.Throws<TokenizeError>(fun () -> tokenize["`"] |> ignore) |> ignore
    

    [<Test>]
    member this.GivenScanner_WhenPassedExpressionConsistingOfInvalidTokens_ThenFailWithScanningError() =
        Assert.Throws<ScanError>(fun () -> scan["?"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["_"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["["][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["]"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["{"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["}"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["£"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["$"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["@"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["<"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan[">"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["%"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["\""][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["!"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["'"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["|"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["\\"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan[":"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan[";"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["#"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["~"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["¬"][] |> ignore) |> ignore
        Assert.Throws<ScanError>(fun () -> scan["`"][] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenScanner_WhenPassedExpressionConsistingOfInvalidWordsOrLetters_ThenFailWithFormatException() =
        Assert.Throws<System.FormatException>(fun () -> scan["a2452Bb"][] |> ignore) |> ignore 
        Assert.Throws<System.FormatException>(fun () -> scan["24tps"][] |> ignore) |> ignore 

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