module Interpreter.Tests.UtilTests

open NUnit.Framework
open Interpreter.Util
    
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