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
        TestCaseData(["-";"-";"-"], [UnaryMinus;UnaryMinus;UnaryMinus])
        TestCaseData(["-";"/";"-"], [UnaryMinus;Divide;UnaryMinus])
        TestCaseData(["-";"(";"3";"+";"4";")";"-";"3"],
                     [UnaryMinus;Lpar;Number 3.0;Plus;Number 4.0;Rpar;Minus;Number 3.0])
    ]

/// <summary>Test cases implicit times when encountering a number then word</summary>
let ImplicitTimesCases =
    [
        TestCaseData(["3";"a"],[Number 3.0; Times; Word "a"])
        TestCaseData(["3";"a";"b"],[Number 3.0; Times; Word "ab"])
        TestCaseData(["3";"c";"e";"i";"l"],[Number 3.0; Times; Function "ceil"])
    ]

/// <summary>Test cases for predefined function inputs to scan.</summary>
let FunctionCases =
    [
        //text to function
        TestCaseData(["c";"e";"i";"l"], [Function "ceil"])
        TestCaseData(["f";"l";"o";"o";"r"], [Function "floor"])
        TestCaseData(["s";"q";"r";"t"], [Function "sqrt"])
        TestCaseData(["c";"b";"r";"t"], [Function "cbrt"])
        TestCaseData(["r";"o";"u";"n";"d"], [Function "round"])
        TestCaseData(["p";"l";"o";"t"], [Function "plot"])
        
        //text to function with parenthesis
        TestCaseData(["c";"e";"i";"l";"(";")"], [Function "ceil";Lpar;Rpar])
        TestCaseData(["f";"l";"o";"o";"r";"(";")"], [Function "floor";Lpar;Rpar])
        
        ///text to function with parenthesis and spaces
        TestCaseData(["c";"e";"i";"l";" ";"(";")"], [Function "ceil";Lpar;Rpar])
        TestCaseData(["f";"l";"o";"o";"r";" ";"(";")"], [Function "floor";Lpar;Rpar])
        
        //text to word to function with parenthesis
        TestCaseData(["t";"e";"s";"t";"(";")"], [Function "test";Lpar;Rpar])
        TestCaseData(["n";"o";"t";"A";"F";"u";"n";"c";"t";"i";"o";"n";"(";")"], [Function "notAFunction";Lpar;Rpar])
        
    ]

/// <summary>Test cases for assign inputs to scan.</summary>
let AssignCases =
    [
        TestCaseData(["W";"o";"r";"d";"-";">";"5";"4"],[Word "Word"; Assign; Number 54.0])
        TestCaseData(["-";"-";">";"5";"4"],[UnaryMinus; Assign; Number 54.0])
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
        TestCaseData(["W";"o";"r";"d";" ";"W";"o";"r";"d"], [Word "Word"; Word "Word"])
        TestCaseData(["W";"o";"r";"d";"5";"W";"o";"r";"d"], [Word "Word"; Number 5.0; Times; Word "Word"])
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
        TestCaseData([".";"2"], [Number 0.2])
        TestCaseData([".";"2";"5"], [Number 0.25])
        TestCaseData([".";"2";"5";"2"], [Number 0.252])
    ]

/// <summary>Test cases for number then word inputs to scan.</summary>
let NumberWordCases =
    [
        TestCaseData(["1";"0";"W";"o";"r";"d"], [Number 10.0; Times; Word "Word"])
    ]

/// <summary>Test cases to ensure that lexer returns correct output with valid input.</summary>
let LexerCases =
    [
        TestCaseData(([]: string list), ([]: terminal list))
        TestCaseData([""], ([]: terminal list))
        TestCaseData(["1"; "0"; "+"; "1";], [Number 10.0; Plus; Number 1.0;])
    ]

[<TestCaseSource(nameof FunctionCases)>]
[<TestCaseSource(nameof LexerCases)>]
[<TestCaseSource(nameof ImplicitTimesCases)>]
[<TestCaseSource(nameof UnaryCases)>]
[<TestCaseSource(nameof WordCases)>]
[<TestCaseSource(nameof NumberCases)>]
[<TestCaseSource(nameof NumberWordCases)>]
[<TestCaseSource(nameof OperatorCases)>]
[<TestCaseSource(nameof AssignCases)>]
let GivenLexer_WhenPassedValidCharacterList_ReturnCorrectTerminals(characters: string list, terminals: terminal list) =
    let result = lexer characters
    Assert.That(result, Is.EqualTo(terminals))

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
    ]
    
/// <summary>Test to ensure that lexer correctly throws an exception for invalid input.</summary>
[<TestCaseSource(nameof ErrorCases)>]
let GivenLexer_WhenPassedCharactersRepresentingInvalidExpression_RaiseTokenizeError(characters: string list) =
    Assert.Throws<TokenizeError>(fun () -> lexer characters |> ignore) |> ignore