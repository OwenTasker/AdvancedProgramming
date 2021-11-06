module Interpreter.Tests.UtilTests

open NUnit.Framework
open Interpreter.Util
    
let validStrContainsOnlyNumberInputs = [
    TestCaseData(".7")
    TestCaseData("1")
    TestCaseData("1.0")
    TestCaseData("1.7")
]

[<TestCaseSource("validStrContainsOnlyNumberInputs")>]
let givenStrContainsOnlyNumberInputs_WhenPassedValidInput_ReturnTrue (input:string) =
    Assert.True(strContainsOnlyNumber input)

let invalidStrContainsOnlyNumberInputs = [
    TestCaseData("a")
    TestCaseData(",")
    TestCaseData(".")
    TestCaseData("")
]
[<TestCaseSource("invalidStrContainsOnlyNumberInputs")>]
let givenStrContainsOnlyNumberInputs_WhenPassedInvalidInput_ReturnFalse (input:string) =
    Assert.False(strContainsOnlyNumber input)
    
let terminalsToStringInputAndOutput = [
    TestCaseData(Plus, "+")
    TestCaseData(UnaryPlus, "+")
    TestCaseData(Minus, "-")
    TestCaseData(UnaryMinus, "-")
    TestCaseData(Times, "*")
    TestCaseData(Divide, "/")
    TestCaseData(Exponent, "^")
    TestCaseData(Lpar, "(")
    TestCaseData(Rpar, ")")
    TestCaseData(Assign, "->")
    TestCaseData(Comma, ",")
    TestCaseData(Function "This", "This")
    TestCaseData(Word "This", "This")
    TestCaseData(Number 5.5, "5.5")
]

[<TestCaseSource("terminalsToStringInputAndOutput")>]
let givenIndividualTerminalToString_WhenProvidedTerminal_ReturnCorrectString(input:terminal, output:string) =
    Assert.That(individualTerminalToString(input), Is.EqualTo(output))