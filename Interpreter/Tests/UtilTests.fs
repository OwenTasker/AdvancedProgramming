/// <summary>
/// Module containing tests for the functions defined in Interpreter.Util.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter.Tests</summary>
/// </namespacedoc>
module Interpreter.Tests.UtilTests

open System.Collections.Generic
open NUnit.Framework
open Interpreter.Util

/// <summary>Test cases for valid input to getPrecedence.</summary>
let ValidGetPrecedenceData =
    [
        TestCaseData(UnaryMinus, 4)
        TestCaseData(UnaryPlus, 4)
        TestCaseData(Exponent, 3)
        TestCaseData(Times, 2)
        TestCaseData(Divide, 2)
        TestCaseData(Plus, 1)
        TestCaseData(Minus, 1)
    ]

/// <summary>Test cases for valid input to getAssociativity.</summary>
let ValidGetAssociativityData =
    [
        TestCaseData(UnaryMinus, "r")
        TestCaseData(UnaryPlus, "r")
        TestCaseData(Exponent, "r")
        TestCaseData(Times, "l")
        TestCaseData(Divide, "l")
        TestCaseData(Plus, "l")
        TestCaseData(Minus, "l")
    ]

/// <summary>Test to ensure that getPrecedence performs correctly under valid input.</summary>
[<TestCaseSource(nameof ValidGetPrecedenceData)>]
let GivenGetPrecedence_WhenPassedOperatorWithPrecedence_ReturnCorrectPrecedence(operator: terminal, precedence: int) =
    let result = getPrecedence operator
    Assert.That(result, Is.EqualTo(precedence))

/// <summary>Test to ensure that getAssociativity performs correctly under valid input.</summary>
[<TestCaseSource(nameof ValidGetAssociativityData)>]
let GivenGetAssociativity_WhenPassedOperatorWithAssociativity_ReturnCorrectAssociativity(operator: terminal, associativity: string) =
    let result = getAssociativity operator
    Assert.That(result, Is.EqualTo(associativity))

/// <summary>Test cases for invalid input to getPrecedence and getAssociativity.</summary>
let InvalidGetPrecedenceAssociativityCases =
    [
        TestCaseData(Lpar)
        TestCaseData(Rpar)
        TestCaseData(Number 1.0)
    ]

/// <summary>Test to ensure that getPrecedence correctly throws exception with invalid input.</summary>
[<TestCaseSource(nameof InvalidGetPrecedenceAssociativityCases)>]
let GivenGetPrecedence_WhenPassedOperatorNotInMap_RaiseKeyNotFoundException(operator: terminal) =
    Assert.Throws<KeyNotFoundException>(fun () -> getPrecedence operator |> ignore) |> ignore

/// <summary>Test to ensure that getAssociativity correctly throws exception with invalid input.</summary>
[<TestCaseSource(nameof InvalidGetPrecedenceAssociativityCases)>]
let GivenGetAssociativity_WhenPassedOperatorNotInMap_RaiseKeyNotFoundException(operator: terminal) =
    Assert.Throws<KeyNotFoundException>(fun () -> getAssociativity operator |> ignore) |> ignore

/// <summary>List of test cases for converting terminals to strings.</summary>
let terminalsToStringInputAndOutput = [
    TestCaseData([Plus], "+")
    TestCaseData([UnaryPlus], "+")
    TestCaseData([Minus], "-")
    TestCaseData([UnaryMinus], "-")
    TestCaseData([Times], "*")
    TestCaseData([Divide], "/")
    TestCaseData([Exponent], "^")
    TestCaseData([Lpar], "(")
    TestCaseData([Rpar], ")")
    TestCaseData([Assign], "->")
    TestCaseData([Comma], ",")
    TestCaseData([Function "This"], "This")
    TestCaseData([Word "This"], "This")
    TestCaseData([Number 5.5], "5.5")
    TestCaseData([Number 5.5;Comma;Number 3.0], "5.5,3")
    TestCaseData([Number 5.5;Plus;Number 5.5], "5.5+5.5")
    TestCaseData([Number 5.5;Plus;UnaryPlus;UnaryMinus;Number 5.5;Minus;Word "x";Times;Function "floor";Lpar
                  Number 3.2;Rpar;], "5.5++-5.5-x*floor(3.2)")
    TestCaseData(([]:terminal list), "")
]

/// <summary>Test to ensure that individualTerminalToString returns the correct string.</summary>
[<TestCaseSource(nameof terminalsToStringInputAndOutput)>]
let givenTerminalListToString_WhenProvidedTerminal_ReturnCorrectString(input:terminal list, output:string) =
    Assert.That(terminalListToString "" input, Is.EqualTo(output))
    
let terminalToNumValidInputsOutputs =
    [
        TestCaseData(Number 0.0, 0.0)
        TestCaseData(Number -0.0, 0.0)
        TestCaseData(Number -100.0, -100.0)
        TestCaseData(Number 100.0, 100.0)
        TestCaseData(Number 100.0, 100.0)
    ]

let terminalToNumInvalidCases =
    [
        TestCaseData(Word "Invalid")
        TestCaseData(Function "Invalid")
        TestCaseData(Comma)
        TestCaseData(Lpar)
        TestCaseData(Plus)
        TestCaseData(Times)
    ]

[<TestCaseSource(nameof terminalToNumInvalidCases)>]
let givenTerminalToNum_ProvidedInvalidInput_ThrowInvalidArgumentError input =
    Assert.Throws<InvalidArgumentError>(fun () -> terminalToNum input |> ignore) |> ignore
    
let toMapCases =
    TestCaseData