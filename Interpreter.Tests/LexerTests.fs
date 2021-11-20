/// <summary>
/// Module containing tests for the functions defined in Interpreter.Lexer.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter.Tests</summary>
/// </namespacedoc>
module Interpreter.Tests.LexerTests

open NUnit.Framework
open Interpreter.Lexer
open Interpreter.Util

/// <summary>List of test cases for valid inputs to tokenize.</summary>
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
        TestCaseData(["+"], ["+"])
        TestCaseData(["*"], ["*"])
        TestCaseData(["/"], ["/"])
        TestCaseData(["-"], ["-"])
        TestCaseData(["^"], ["^"])
        TestCaseData(["("], ["("])
        TestCaseData([")"], [")"])
        TestCaseData([">"], [">"])
        TestCaseData(["-";">"], ["->"])
        TestCaseData(["+";"+"], ["+";"+"])
        TestCaseData(["*";"*"], ["*";"*"])
        TestCaseData(["+";"*"], ["+";"*"])
        TestCaseData(["+";"*";"/"], ["+";"*";"/"])
        TestCaseData(["a"], ["a"])
        TestCaseData(["A"], ["A"])
        TestCaseData(["a";"b"], ["ab"])
        TestCaseData(["A";"B"], ["AB"])
        TestCaseData(["a";"b";"c"], ["abc"])
        TestCaseData(["1";"+";"1";"*";"1";"/";"1";"-";"1";"^";"1";"(";"1";")"],
                     ["1";"+";"1";"*";"1";"/";"1";"-";"1";"^";"1";"(";"1";")"])
        TestCaseData(["T";"h";"i";"s";" ";"i";"s";" ";"a";" ";"s";"e";"n";"t";"e";"n";"c";"e"],
                     ["This";"is";"a";"sentence"])
        TestCaseData(["1";".";"2";"5";"+";"1"], ["1.25"; "+"; "1"])
        TestCaseData(["1";".";"2";"5";"+";"1";"2";"5"], ["1.25"; "+"; "125"])
        TestCaseData([".";"2";"5";"+";"1";"2";"5"], [".25"; "+"; "125"])
        TestCaseData(["T";"h";"i";"s";" ";"-";">";"2";"x"],["This";"->";"2";"x"])
    ]

/// <summary>Test to ensure that tokenize returns the correct output for valid inputs.</summary>
[<TestCaseSource("TokenizeTestData")>]
let GivenTokenize_WhenPassedInput_ReturnCorrectTokens(op1, res) =
    let result = tokenize op1
    Assert.That(result, Is.EqualTo(res))

/// <summary>Test cases for operator inputs to scan.</summary>
let OperatorCases =
    [
        TestCaseData(["+"],[UnaryPlus])
        TestCaseData([","],[Comma])
        TestCaseData(["-"],[UnaryMinus])
        TestCaseData(["3";"+"],[Number 3.0;Plus])
        TestCaseData(["3";"-"],[Number 3.0;Minus])
        TestCaseData(["^"],[Exponent])
        TestCaseData(["*"],[Times])
        TestCaseData(["("],[Lpar])
        TestCaseData([")"],[Rpar])
        TestCaseData(["/"],[Divide])
        TestCaseData(["->"],[Assign])
    ]

/// <summary>Test cases for unary inputs to scan.</summary> 
let UnaryCases =   
    [
        TestCaseData(["+";"+";"+"], [UnaryPlus;UnaryPlus;UnaryPlus])
        TestCaseData(["-";"-";"-"], [UnaryMinus;UnaryMinus;UnaryMinus])
        TestCaseData(["-";"(";"3";"+";"4";")";"-";"3"],
                     [UnaryMinus;Lpar;Number 3.0;Plus;Number 4.0;Rpar;Minus;Number 3.0])
    ]
    
/// <summary>Test cases for predefined function inputs to scan.</summary>
let FunctionCases =
    [
        TestCaseData(["ceil"], [Function "ceil"])
        TestCaseData(["floor"], [Function "floor"])
        TestCaseData(["sqrt"], [Function "sqrt"])
        TestCaseData(["round"], [Function "round"])
        TestCaseData(["ceil"; "3.222"; "floor"; "3.222"],
                     [Function "ceil"; Number 3.222; Function "floor"; Number 3.222])
    ]
    
/// <summary>Test cases for assign inputs to scan.</summary>
let AssignCases =
    [
        TestCaseData(["Word";"->";"54"],[Word "Word"; Assign; Number 54.0])
        TestCaseData(["->";"->";"->";],[Assign; Assign; Assign])
        TestCaseData(["->";"-";"-";"->";],[Assign; UnaryMinus; UnaryMinus; Assign])
        TestCaseData(["54";"->";"Word";],[Number 54.0; Assign; Word "Word"])
    ]

/// <summary>Test cases for word inputs to scan.</summary>
let WordCases =
    [
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
        TestCaseData(["Word";"5";"Word"], [Word "Word"; Number 5.0; Times; Word "Word"])
    ]

/// <summary>Test cases for number inputs to scan.</summary>
let NumberCases =
    [
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
    
let NumberWordCases =
    [
        TestCaseData(["10";"Word"], [Number 10.0; Times; Word "Word"])
    ]
    
let NumberFunctionCases =
    [
        TestCaseData(["10";"ceil"], [Number 10.0; Function "ceil"])
    ]

/// <summary>Test to ensure that scan returns the correct output for valid inputs.</summary>
[<TestCaseSource("OperatorCases")>]
[<TestCaseSource("UnaryCases")>]
[<TestCaseSource("NumberWordCases")>]
[<TestCaseSource("NumberFunctionCases")>]
[<TestCaseSource("FunctionCases")>]
[<TestCaseSource("AssignCases")>]
[<TestCaseSource("WordCases")>]
[<TestCaseSource("NumberCases")>]
let GivenScan_WhenPassedInput_ReturnCorrectTokens(op1, res) =
    let result = scan op1 []
    Assert.That(result, Is.EqualTo(res))

/// <summary>List of test cases for invalid inputs to tokenize, scan and lexer.</summary>
let ErrorCases =
    [
        TestCaseData(["?"])
        TestCaseData(["_"])
        TestCaseData(["["])
        TestCaseData(["]"])
        TestCaseData(["{"])
        TestCaseData(["}"])
        TestCaseData(["£"])
        TestCaseData(["$"])
        TestCaseData(["@"])
        TestCaseData(["="])
        TestCaseData(["<"])
        TestCaseData(["%"])
        TestCaseData(["\""])
        TestCaseData(["!"])
        TestCaseData(["'"])
        TestCaseData(["|"])
        TestCaseData(["\\"])
        TestCaseData([":"])
        TestCaseData([";"])
        TestCaseData(["#"])
        TestCaseData(["~"])
        TestCaseData(["¬"])
        TestCaseData(["`"])
        TestCaseData(["1"; "?"])
        TestCaseData(["1"; "?"])
    ]

/// <summary>Test to ensure that tokenize correctly throws an exception for invalid input.</summary>
[<TestCaseSource("ErrorCases")>]
let GivenTokenize_WhenPassedInvalidCharacter_ThenRaiseTokenizeError(characters: string list) =
    Assert.Throws<TokenizeError>(fun () -> tokenize characters |> ignore) |> ignore

/// <summary>Test to ensure that scan correctly throws an exception for invalid input.</summary>
[<TestCaseSource("ErrorCases")>]
let GivenScan_WhenPassedInvalidCharacter_ThenRaiseScanError(token: string list) =
    Assert.Throws<ScanError>(fun () -> scan token [] |> ignore) |> ignore
    
let FormatExceptionCases =
    [
        TestCaseData("a2452Bb")
        TestCaseData("24tps")
    ]

/// <summary>Test to ensure that scan correctly throws a FormatException for invalid input.</summary>
[<TestCaseSource("FormatExceptionCases")>]
let GivenScan_WhenPassedExpressionConsistingOfInvalidWordsOrLetters_ThenFailWithFormatException(token: string) =
    Assert.Throws<System.FormatException>(fun () -> scan [token] [] |> ignore) |> ignore

/// <summary>Test cases to ensure that lexer returns correct output with valid input.</summary>
let LexerCases =
    [
        TestCaseData(([]: string list), ([]: terminal list))
        TestCaseData([""], ([]: terminal list))
        TestCaseData(["1"; "0"; "+"; "1";], [Number 10.0; Plus; Number 1.0;])
    ]

/// <summary>Test to ensure that lexer returns correct output with valid input.</summary>
[<TestCaseSource("LexerCases")>]
let GivenLexer_WhenPassedValidCharacterList_ReturnCorrectTerminals(characters: string list, terminals: terminal list) =
    let result = lexer characters
    Assert.That(result, Is.EqualTo(terminals))
    
/// <summary>Test to ensure that lexer correctly throws an exception for invalid input.</summary>
[<TestCaseSource("ErrorCases")>]
let GivenLexer_WhenPassedCharactersRepresentingInvalidExpression_RaiseTokenizeError(characters: string list) =
    Assert.Throws<TokenizeError>(fun () -> lexer characters |> ignore) |> ignore
