module Interpreter.Tests.PublicInterfaceTests

open NUnit.Framework
open Interpreter.Util
open Interpreter.PublicInterface
open NUnit.Framework

/// <summary>Example environment for testing.</summary>
let env =
    Map [ ("x", [ Number 2.0 ])
          ("y", [ Number 1.0; Plus; Word "x" ])
          ("z", [ Word "a" ])
          ("q", [ Word "x"; Plus; Word "z" ]) ]

/// <summary>Test cases for valid inputs to closed.</summary>
let ClosedCases =
    [ TestCaseData(([]: string list), true)
      TestCaseData([ "1" ], true)
      TestCaseData([ "x" ], true)
      TestCaseData([ "x"; "+"; "1" ], true)
      TestCaseData([ "x"; "+"; "y" ], true)
      TestCaseData([ "z" ], false)
      TestCaseData([ "q" ], false)
      TestCaseData([ "x"; "+"; "z" ], false) ]

/// <summary>Test to ensure that closed correctly recognises closed and free expressions.</summary>
[<TestCaseSource(nameof ClosedCases)>]
let GivenClosed_WhenPassedExpression_ReturnCorrectBoolean (expression: string list, expected: bool) =
    let result = closed expression env
    Assert.That(result, Is.EqualTo(expected))

let StringToTerminalListValidCases =
    [
        TestCaseData(["1";], [Number 1.0;])
        TestCaseData(["x"; "^"; "x"], [Word "x"; Exponent; Word "x"])
        TestCaseData(["1"; "+"; "1"; "-"; "3"; "6"; "2"; "."; "1"; "9"; "*"; "-"; "y"; "/"; "f"; "u"; "n"; "c"; "("; "0"
                      "^"; "+"; "z"; "a"; "p"; ")"],
                     [Number 1.0; Plus; Number 1.0; Minus; Number 362.19; Times; UnaryMinus; Word "y"; Divide
                      Function "func"; Lpar; Number 0.0; Exponent; UnaryPlus; Word "zap"; Rpar;])
    ]

[<TestCaseSource(nameof StringToTerminalListValidCases)>]
let GivenStringToTerminalList_WhenProvidedValidInput_ReturnOutputAsTerminalList(input: string list, expected: terminal list) =
    let result = stringToTerminalList input
    Assert.That(result, Is.EqualTo(expected))
