/// <summary>
/// Module containing tests for the functions defined in Interpreter.Exec.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter.Tests</summary>
/// </namespacedoc>
module Interpreter.Tests.ExecTests

open NUnit.Framework
open Interpreter.Exec
open Interpreter.Util

/// <summary>Test cases for valid input to reduce, reduceRecursive and exec without variables.</summary>
let SimpleAdditionCases =
    [ TestCaseData([ Number 1.0; Plus; Number 1.0 ], Number 2.0)
      TestCaseData([ Number 1.0; Plus; Number 0.0 ], Number 1.0)
      TestCaseData([ Number 0.0; Plus; Number 67.0 ], Number 67.0)
      TestCaseData([ Number 0.0; Plus; Number 0.0 ], Number 0.0)
      TestCaseData([ Number 5.0; Plus; Number -5.0 ], Number 0.0)
      TestCaseData([ Number -5.0; Plus; Number -5.0 ], Number -10.0)
      TestCaseData([ Number -5.0; Plus; Number 5.0 ], Number 0.0)
      TestCaseData([ Number 10.0; Plus; Number -100.0 ], Number -90.0) ]

let SimpleSubtractionCases =
    [ TestCaseData([ Number 1.0; Minus; Number 1.0 ], Number 0.0)
      TestCaseData([ Number 1.0; Minus; Number 0.0 ], Number 1.0)
      TestCaseData([ Number 0.0; Minus; Number 67.0 ], Number -67.0)
      TestCaseData([ Number 0.0; Minus; Number 0.0 ], Number 0.0)
      TestCaseData([ Number 5.0; Minus; Number -5.0 ], Number 10.0)
      TestCaseData([ Number -5.0; Minus; Number -5.0 ], Number 0.0)
      TestCaseData([ Number -5.0; Minus; Number 5.0 ], Number -10.0)
      TestCaseData([ Number 10.0; Minus; Number -100.0 ], Number 110.0) ]

let SimpleMultiplicationCases =
    [ TestCaseData([ Number 1.0; Times; Number 8.0 ], Number 8.0)
      TestCaseData([ Number 8.0; Times; Number 1.0 ], Number 8.0)
      TestCaseData([ Number 0.0; Times; Number 11.0 ], Number 0.0)
      TestCaseData([ Number 10.0; Times; Number 0.0 ], Number 0.0)
      TestCaseData([ Number 0.0; Times; Number 0.0 ], Number 0.0)
      TestCaseData([ Number -10.0; Times; Number -10.0 ], Number 100.0)
      TestCaseData([ Number 10.0; Times; Number 10.0 ], Number 100.0)
      TestCaseData([ Number -10.0; Times; Number 10.0 ], Number -100.0)
      TestCaseData([ Number 10.0; Times; Number -10.0 ], Number -100.0) ]

let SimpleDivisionCases =
    [ TestCaseData([ Number 1.0; Divide; Number 8.0 ], Number 0.125)
      TestCaseData([ Number 8.0; Divide; Number 1.0 ], Number 8.0)
      TestCaseData([ Number 0.0; Divide; Number 11.0 ], Number 0.0)
      TestCaseData([ Number -10.0; Divide; Number -10.0 ], Number 1.0)
      TestCaseData([ Number 10.0; Divide; Number 10.0 ], Number 1.0)
      TestCaseData([ Number -10.0; Divide; Number 10.0 ], Number -1.0)
      TestCaseData([ Number 10.0; Divide; Number -10.0 ], Number -1.0) ]

let SimpleExponentiationCases =
    [ TestCaseData([ Number 1.0; Exponent; Number 8.0 ], Number 1.0)
      TestCaseData([ Number 8.0; Exponent; Number 1.0 ], Number 8.0)
      TestCaseData([ Number 0.0; Exponent; Number 11.0 ], Number 0.0)
      TestCaseData([ Number 11.0; Exponent; Number 0.0 ], Number 1.0)
      TestCaseData([ Number 0.0; Exponent; Number 0.0 ], Number 1.0)
      TestCaseData([ Number -27.0; Exponent; Number -10.0 ], Number(-27.0 ** -10.0))
      TestCaseData([ Number 10.0; Exponent; Number 2.0 ], Number 100.0)
      TestCaseData([ Number -10.0; Exponent; Number 2.0 ], Number 100.0)
      TestCaseData([ Number 10.0; Exponent; Number 3.0 ], Number 1000.0)
      TestCaseData([ Number -10.0; Exponent; Number 3.0 ], Number -1000.0)
      TestCaseData([ Number 10.0; Exponent; Number -3.0 ], Number 0.001)
      TestCaseData([ Number -10.0; Exponent; Number -3.0 ], Number -0.001) ]

let SimpleUnaryCases =
    [ TestCaseData([ UnaryMinus; Number 1.0 ], Number -1.0)
      TestCaseData([ UnaryMinus; Number -1.0 ], Number 1.0)
      TestCaseData([ UnaryPlus; Number 1.0 ], Number 1.0)
      TestCaseData([ UnaryPlus; Number -1.0 ], Number -1.0) ]

let ChainedUnaryCases =
    [ TestCaseData([ UnaryMinus; UnaryMinus; Number 1.0 ], Number 1.0)
      TestCaseData([ UnaryMinus; UnaryMinus; Number -1.0 ], Number -1.0)
      TestCaseData([ UnaryPlus; UnaryPlus; Number 1.0 ], Number 1.0)
      TestCaseData([ UnaryPlus; UnaryPlus; Number -1.0 ], Number -1.0) ]

let OrderOfOperationsCases =
    [ TestCaseData(
        [ Number 1.0
          Plus
          Number 8.0
          Minus
          Number 6.0 ],
        Number 3.0
      )
      TestCaseData(
          [ Number 1.0
            Minus
            Number 8.0
            Plus
            Number 6.0 ],
          Number -1.0
      )
      TestCaseData(
          [ Number 64.0
            Times
            Number 8.0
            Divide
            Number 4.0 ],
          Number 128.0
      )
      TestCaseData(
          [ Number 64.0
            Divide
            Number 4.0
            Times
            Number 8.0 ],
          Number 128.0
      )
      TestCaseData(
          [ Number 7.0
            Plus
            Number 8.0
            Times
            Number 6.0 ],
          Number 55.0
      )
      TestCaseData(
          [ Number 9.0
            Times
            Number 8.0
            Plus
            Number 6.0 ],
          Number 78.0
      )
      TestCaseData(
          [ Number 6.0
            Times
            Number 8.0
            Minus
            Number 4.0 ],
          Number 44.0
      )
      TestCaseData(
          [ Number 6.0
            Minus
            Number 4.0
            Times
            Number 8.0 ],
          Number -26.0
      )
      TestCaseData(
          [ Number 1.0
            Plus
            Number 8.0
            Divide
            Number 4.0 ],
          Number 3.0
      )
      TestCaseData(
          [ Number 1.0
            Divide
            Number 8.0
            Plus
            Number 6.0 ],
          Number 6.125
      )
      TestCaseData(
          [ Number 64.0
            Divide
            Number 8.0
            Minus
            Number 4.0 ],
          Number 4.0
      )
      TestCaseData(
          [ Number 64.0
            Minus
            Number 4.0
            Divide
            Number 8.0 ],
          Number 63.5
      )
      TestCaseData(
          [ Number 1.0
            Plus
            Number 8.0
            Exponent
            Number 2.0 ],
          Number 65.0
      )
      TestCaseData(
          [ Number 1.0
            Exponent
            Number 8.0
            Plus
            Number 6.0 ],
          Number 7.0
      )
      TestCaseData(
          [ Number 2.0
            Minus
            Number 8.0
            Exponent
            Number 2.0 ],
          Number -62.0
      )
      TestCaseData(
          [ Number 16.0
            Exponent
            Number 2.0
            Minus
            Number 8.0 ],
          Number 248.0
      )
      TestCaseData(
          [ Number 7.0
            Times
            Number 5.0
            Exponent
            Number 3.0 ],
          Number 875.0
      )
      TestCaseData(
          [ Number 9.0
            Exponent
            Number 2.0
            Times
            Number 6.0 ],
          Number 486.0
      )
      TestCaseData(
          [ Number 64.0
            Divide
            Number 8.0
            Exponent
            Number 2.0 ],
          Number 1.0
      )
      TestCaseData(
          [ Number 6.0
            Exponent
            Number 3.0
            Divide
            Number 2.0 ],
          Number 108.0
      )
      TestCaseData(
          [ Lpar
            Number 1.0
            Plus
            Number 8.0
            Rpar
            Minus
            Number 6.0 ],
          Number 3.0
      )
      TestCaseData(
          [ Lpar
            Number 1.0
            Minus
            Number 8.0
            Rpar
            Plus
            Number 6.0 ],
          Number -1.0
      )
      TestCaseData(
          [ Lpar
            Number 64.0
            Times
            Number 8.0
            Rpar
            Divide
            Number 4.0 ],
          Number 128.0
      )
      TestCaseData(
          [ Lpar
            Number 64.0
            Divide
            Number 4.0
            Rpar
            Times
            Number 8.0 ],
          Number 128.0
      )
      TestCaseData(
          [ Lpar
            Number 7.0
            Plus
            Number 8.0
            Rpar
            Times
            Number 6.0 ],
          Number 90.0
      )
      TestCaseData(
          [ Lpar
            Number 9.0
            Times
            Number 8.0
            Rpar
            Plus
            Number 6.0 ],
          Number 78.0
      )
      TestCaseData(
          [ Lpar
            Number 6.0
            Times
            Number 8.0
            Rpar
            Minus
            Number 4.0 ],
          Number 44.0
      )
      TestCaseData(
          [ Lpar
            Number 6.0
            Minus
            Number 4.0
            Rpar
            Times
            Number 8.0 ],
          Number 16.0
      )
      TestCaseData(
          [ Lpar
            Number 1.0
            Plus
            Number 8.0
            Rpar
            Divide
            Number 4.0 ],
          Number 2.25
      )
      TestCaseData(
          [ Lpar
            Number 1.0
            Divide
            Number 8.0
            Rpar
            Plus
            Number 6.0 ],
          Number 6.125
      )
      TestCaseData(
          [ Lpar
            Number 64.0
            Divide
            Number 8.0
            Rpar
            Minus
            Number 4.0 ],
          Number 4.0
      )
      TestCaseData(
          [ Lpar
            Number 64.0
            Minus
            Number 4.0
            Rpar
            Divide
            Number 8.0 ],
          Number 7.5
      )
      TestCaseData(
          [ Lpar
            Number 1.0
            Plus
            Number 8.0
            Rpar
            Exponent
            Number 2.0 ],
          Number 81.0
      )
      TestCaseData(
          [ Lpar
            Number 1.0
            Exponent
            Number 8.0
            Rpar
            Plus
            Number 6.0 ],
          Number 7.0
      )
      TestCaseData(
          [ Lpar
            Number 2.0
            Minus
            Number 8.0
            Rpar
            Exponent
            Number 2.0 ],
          Number 36.0
      )
      TestCaseData(
          [ Lpar
            Number 16.0
            Exponent
            Number 2.0
            Rpar
            Minus
            Number 8.0 ],
          Number 248.0
      )
      TestCaseData(
          [ Lpar
            Number 7.0
            Times
            Number 5.0
            Rpar
            Exponent
            Number 3.0 ],
          Number 42875.0
      )
      TestCaseData(
          [ Lpar
            Number 9.0
            Exponent
            Number 2.0
            Rpar
            Times
            Number 6.0 ],
          Number 486.0
      )
      TestCaseData(
          [ Lpar
            Number 64.0
            Divide
            Number 8.0
            Rpar
            Exponent
            Number 2.0 ],
          Number 64.0
      )
      TestCaseData(
          [ Lpar
            Number 6.0
            Exponent
            Number 3.0
            Rpar
            Divide
            Number 2.0 ],
          Number 108.0
      )
      TestCaseData(
          [ UnaryMinus
            Lpar
            UnaryPlus
            Number 6.0
            Exponent
            UnaryMinus
            Number 3.0
            Rpar
            Divide
            Number 2.0 ],
          Number((-1.0 / 6.0 ** 3.0) / 2.0)
      )
      TestCaseData(
          [ UnaryMinus
            Lpar
            UnaryPlus
            Number 6.0
            Divide
            UnaryMinus
            Number 3.0
            Rpar
            Exponent
            Number 2.0 ],
          Number 4.0
      )
      TestCaseData(
          [ UnaryMinus
            Number 2.0
            Exponent
            Number 3.0 ],
          Number -8.0
      ) ]

/// <summary>Test to ensure that exec returns the correct output with valid input without variables.</summary>
[<TestCaseSource("SimpleAdditionCases")>]
[<TestCaseSource("SimpleSubtractionCases")>]
[<TestCaseSource("SimpleMultiplicationCases")>]
[<TestCaseSource("SimpleDivisionCases")>]
[<TestCaseSource("SimpleExponentiationCases")>]
[<TestCaseSource("SimpleUnaryCases")>]
[<TestCaseSource("ChainedUnaryCases")>]
[<TestCaseSource("OrderOfOperationsCases")>]
let GivenExec_WhenPassedSimpleExpressionWithoutVariables_ReturnCorrectAnswer
    (
        tokens: terminal list,
        expected: terminal
    ) =
    let result = exec Map.empty tokens
    Assert.That(result, Is.EqualTo([ expected ], Map.empty |> Map.toSeq |> dict))

/// <summary>Test cases for invalid input to reduce and reduceRecursive.</summary>
let InvalidReduceCases =
    [ TestCaseData([ Comma ])
      TestCaseData([ Plus ])
      TestCaseData([ Minus ])
      TestCaseData([ Times ])
      TestCaseData([ Divide ])
      TestCaseData([ Exponent ])
      TestCaseData([ Lpar ])
      TestCaseData([ Rpar ])
      TestCaseData([ UnaryPlus ])
      TestCaseData([ UnaryMinus ])
      TestCaseData([ Lpar; Rpar ])
      TestCaseData([ Lpar; Plus; Rpar ])
      TestCaseData([ Plus; Number 5.0 ])
      TestCaseData([ Number 1.0; Plus ])
      TestCaseData([ Minus; Number 5.0 ])
      TestCaseData([ Number 1.0; Minus ])
      TestCaseData([ Times; Number 5.0 ])
      TestCaseData([ Number 1.0; Times ])
      TestCaseData([ Divide; Number 5.0 ])
      TestCaseData([ Number 1.0; Divide ])
      TestCaseData([ Lpar; Number 1.0; Plus; Number 1.0 ])
      TestCaseData([ Lpar; Number 1.0; Plus; Number 1.0 ])
      TestCaseData(
          [ Number 5.0
            Lpar
            Number 5.0
            Plus
            Number 6.0
            Rpar ]
      )
      TestCaseData(
          [ Lpar
            Number 5.0
            Plus
            Number 6.0
            Rpar
            Number 5.0 ]
      )
      TestCaseData([ Number 2.0; Exponent ])
      TestCaseData([ Exponent; Number 5.0 ])
      TestCaseData([ Assign ])
      TestCaseData([ Word "x"; Assign ]) ]

/// <summary>Test to ensure that exec returns the correct output with invalid input.</summary>
[<TestCaseSource("InvalidReduceCases")>]
let GivenExed_WhenPassedInvalidTokens_RaiseExecError (tokens: terminal list) =
    Assert.Throws<ExecError>(fun () -> exec Map.empty tokens |> ignore)
    |> ignore

/// <summary>Example environment for testing.</summary>
let env =
    Map [ ("x", [ Number 2.0 ])
          ("y", [ Number 1.0; Plus; Word "x" ])
          ("z", [ Word "a" ])
          ("q", [ Word "x"; Plus; Word "z" ]) ]

/// <summary>Test cases for valid inputs to closed.</summary>
let ClosedCases =
    [ TestCaseData(([]: terminal list), true)
      TestCaseData([ Number 1.0 ], true)
      TestCaseData([ Word "x" ], true)
      TestCaseData([ Word "x"; Plus; Number 1.0 ], true)
      TestCaseData([ Word "x"; Plus; Word "y" ], true)
      TestCaseData([ Word "z" ], false)
      TestCaseData([ Word "q" ], false)
      TestCaseData([ Word "x"; Plus; Word "z" ], false) ]

/// <summary>Test to ensure that closed correctly recognises closed and free expressions.</summary>
[<TestCaseSource("ClosedCases")>]
let GivenClosed_WhenPassedExpression_ReturnCorrectBoolean (terminals: terminal list, expected: bool) =
    let result = closed env terminals
    Assert.That(result, Is.EqualTo(expected))

/// <summary>Test cases for valid assign inputs to exec.</summary>
let ValidExecAssignCases =
    [ TestCaseData([ Word "x"; Assign; Number 2.0 ], ("x", [ Number 2.0 ]))
      TestCaseData([ Word "x"; Assign; Word "x" ], ("x", [ Number 2.0 ]))
      TestCaseData(
          [ Word "x"
            Assign
            Word "x"
            Plus
            Word "x" ],
          ("x", [ Number 4.0 ])
      )
      TestCaseData([ Word "y"; Assign; Word "z" ], ("y", [ Word "z" ]))
      TestCaseData(
          [ Word "b"
            Assign
            Word "x"
            Times
            Word "y" ],
          ("b", [ Number 6.0 ])
      )
      TestCaseData(
          [ Word "x"
            Assign
            Word "x"
            Plus
            Number 1.0 ],
          ("x", [ Number 3.0 ])
      ) ]

/// <summary>Test to ensure that exec correctly updates environment when passed valid assign.</summary>
[<TestCaseSource("ValidExecAssignCases")>]
let GivenExec_WhenPassedValidAssign_ThenAddToEnvAndReturn (terminals: terminal list, entry: string * terminal list) =
    let result = exec env terminals
    Assert.That(result, Is.EqualTo((terminals, (env.Add entry))))

/// <summary>Test cases for valid user function calls to exec.</summary>
let UserFunctionCases =
    [ TestCaseData([ Function "y"; Lpar; Rpar ], Map [ ("y", [ Number 2.0 ]) ], [ Number 2.0 ])
      TestCaseData(
          [ Function "y"; Lpar; Rpar ],
          Map [ ("y", [ Word "x" ])
                ("x", [ Number 2.0 ]) ],
          [ Number 2.0 ]
      )
      TestCaseData(
          [ Function "y"
            Lpar
            Word "x"
            Assign
            Number 2.0
            Rpar ],
          Map [ ("y", [ Word "x" ])
                ("x", [ Number 2.0 ]) ],
          [ Number 2.0 ]
      )
      TestCaseData(
          [ Function "y"
            Lpar
            Word "x"
            Assign
            Number 4.0
            Rpar ],
          Map [ ("y", [ Word "x" ]) ],
          [ Number 4.0 ]
      )
      TestCaseData(
          [ Function "y"
            Lpar
            Word "x"
            Assign
            Number 4.0
            Rpar ],
          Map [ ("y", [ Word "x"; Exponent; Number 2.0 ]) ],
          [ Number 16.0 ]
      )
      TestCaseData(
          [ Function "y"
            Lpar
            Word "x"
            Assign
            Number 4.0
            Comma
            Word "z"
            Assign
            Number 3.0
            Rpar ],
          Map [ ("y",
                 [ Word "x"
                   Exponent
                   Number 2.0
                   Plus
                   Word "z" ]) ],
          [ Number 19.0 ]
      ) ]

/// <summary>Test to ensure that exec returns correctly result with valid user function call.</summary>
[<TestCaseSource("UserFunctionCases")>]
let GivenExec_WhenPassedValidUserFunctionCall_ReturnCorrectResult
    (
        terminals: terminal list,
        env: Map<string, terminal list>,
        expected: terminal list
    ) =
    let result = exec env terminals
    Assert.That(result, Is.EqualTo((expected, env |> Map.toSeq |> dict)))

/// <summary>Test cases for invalid input to exec with user functions.</summary>
let UserFunctionErrorCases =
    [ TestCaseData([ Function "y"; Lpar ], Map [ ("y", [ Number 2.0 ]) ])
      TestCaseData([ Function "y"; Lpar; Comma; Rpar ], Map [ ("y", [ Word "x" ]) ])
      TestCaseData(
          [ Function "y"; Lpar; Rpar ],
          Map [ ("y", [ Word "x" ])
                ("x", [ Word "z" ]) ]
      )
      TestCaseData(
          [ Function "y"
            Lpar
            Word "x"
            Assign
            Word "z"
            Rpar ],
          Map [ ("y", [ Word "x" ])
                ("x", [ Number 2.0 ]) ]
      )
      TestCaseData(
          [ Function "y"
            Lpar
            Word "x"
            Assign
            Number 4.0
            Rpar ],
          Map [ ("y",
                 [ Word "x"
                   Exponent
                   Number 2.0
                   Plus
                   Word "z" ]) ]
      )
      TestCaseData(
          [ Function "y"
            Lpar
            Word "x"
            Assign
            Number 4.0
            Comma ],
          Map [ ("y",
                 [ Word "x"
                   Exponent
                   Number 2.0
                   Plus
                   Word "z" ]) ]
      ) ]

/// <summary>Test to ensure that exec correctly throws exception with invalid user function call.</summary>
[<TestCaseSource("UserFunctionErrorCases")>]
let GivenExec_WhenPassedInvalidUserFunctionCall_RaiseExecError
    (
        terminals: terminal list,
        env: Map<string, terminal list>
    ) =
    Assert.Throws<ExecError>(fun () -> exec env terminals |> ignore)
    |> ignore

let SystemFunctionCases =
    [ TestCaseData(
        [ Function "sqrt"
          Lpar
          Number 4.0
          Rpar ],
        [ Number 2.0 ]
      )
      TestCaseData(
          [ Function "sqrt"
            Lpar
            Number 0.0
            Rpar ],
          [ Number 0.0 ]
      )
      TestCaseData(
          [ Function "sqrt"
            Lpar
            Word "x"
            Plus
            Number 1.0
            Rpar ],
          [ Number(3.0 ** (1.0 / 2.0)) ]
      )
      TestCaseData(
          [ Number 2.0
            Plus
            Function "sqrt"
            Lpar
            Number 4.0
            Rpar ],
          [ Number 4.0 ]
      )
      TestCaseData(
          [ Function "sqrt"
            Lpar
            Number 4.0
            Rpar
            Plus
            Number 5.7 ],
          [ Number 7.7 ]
      )
      TestCaseData(
          [ Function "cbrt"
            Lpar
            Number 8.0
            Rpar ],
          [ Number 2.0 ]
      )
      TestCaseData(
          [ Function "cbrt"
            Lpar
            Number -27.0
            Rpar ],
          [ Number -3.0 ]
      )
      TestCaseData(
          [ Function "cbrt"
            Lpar
            Number 0.0
            Rpar ],
          [ Number 0.0 ]
      )
      TestCaseData(
          [ Function "cbrt"
            Lpar
            Number 3.0
            Times
            Word "x"
            Rpar ],
          [ Number(6.0 ** (1.0 / 3.0)) ]
      )
      TestCaseData(
          [ Number 2.0
            Plus
            Function "cbrt"
            Lpar
            Number 8.0
            Rpar ],
          [ Number 4.0 ]
      )
      TestCaseData(
          [ Function "cbrt"
            Lpar
            Number 8.0
            Rpar
            Plus
            Number 5.7 ],
          [ Number 7.7 ]
      )
      TestCaseData(
          [ Function "xrt"
            Lpar
            Number 8.0
            Comma
            Number 3.0
            Rpar ],
          [ Number 2.0 ]
      )
      TestCaseData(
          [ Function "xrt"
            Lpar
            Number 0.0
            Comma
            Number 8.0
            Rpar ],
          [ Number 0.0 ]
      )
      TestCaseData(
          [ Function "xrt"
            Lpar
            Number 125.0
            Comma
            Number -3.0
            Rpar ],
          [ Number 0.2 ]
      )
      TestCaseData(
          [ Function "xrt"
            Lpar
            Number -64.0
            Comma
            Number 1.0
            Rpar ],
          [ Number -64.0 ]
      )
      TestCaseData(
          [ Function "xrt"
            Lpar
            Number 8.0
            Comma
            Word "x"
            Plus
            Number 1.0
            Rpar ],
          [ Number 2.0 ]
      )
      TestCaseData(
          [ Function "xrt"
            Lpar
            Number 8.0
            Comma
            Number 3.0
            Rpar
            Plus
            Number 5.7 ],
          [ Number 7.7 ]
      )
      TestCaseData(
          [ Number 5.7
            Plus
            Function "xrt"
            Lpar
            Number 8.0
            Comma
            Number 3.0
            Rpar ],
          [ Number 7.7 ]
      )
      TestCaseData(
          [ Function "floor"
            Lpar
            Number 8.0
            Rpar ],
          [ Number 8.0 ]
      )
      TestCaseData(
          [ Function "floor"
            Lpar
            Number -27.0
            Rpar ],
          [ Number -27.0 ]
      )
      TestCaseData(
          [ Function "floor"
            Lpar
            Number 8.5
            Rpar ],
          [ Number 8.0 ]
      )
      TestCaseData(
          [ Function "floor"
            Lpar
            Number -3.3
            Rpar ],
          [ Number -4.0 ]
      )
      TestCaseData(
          [ Number 2.0
            Plus
            Function "floor"
            Lpar
            Number 8.1
            Rpar ],
          [ Number 10.0 ]
      )
      TestCaseData(
          [ Function "floor"
            Lpar
            Number 8.1
            Rpar
            Plus
            Number 5.7 ],
          [ Number 13.7 ]
      )
      TestCaseData(
          [ Function "ceil"
            Lpar
            Number 8.0
            Rpar ],
          [ Number 8.0 ]
      )
      TestCaseData(
          [ Function "ceil"
            Lpar
            Number -27.0
            Rpar ],
          [ Number -27.0 ]
      )
      TestCaseData(
          [ Function "ceil"
            Lpar
            Number 8.5
            Rpar ],
          [ Number 9.0 ]
      )
      TestCaseData(
          [ Function "ceil"
            Lpar
            Number -3.3
            Rpar ],
          [ Number -3.0 ]
      )
      TestCaseData(
          [ Number 2.0
            Plus
            Function "ceil"
            Lpar
            Number 8.1
            Rpar ],
          [ Number 11.0 ]
      )
      TestCaseData(
          [ Function "ceil"
            Lpar
            Number 8.1
            Rpar
            Plus
            Number 5.7 ],
          [ Number 14.7 ]
      )
      TestCaseData(
          [ Function "round"
            Lpar
            Number 8.0
            Rpar ],
          [ Number 8.0 ]
      )
      TestCaseData(
          [ Function "round"
            Lpar
            Number -27.0
            Rpar ],
          [ Number -27.0 ]
      )
      TestCaseData(
          [ Function "round"
            Lpar
            Number 8.5
            Rpar ],
          [ Number 9.0 ]
      )
      TestCaseData(
          [ Function "round"
            Lpar
            Number -3.3
            Rpar ],
          [ Number -3.0 ]
      )
      TestCaseData(
          [ Function "round"
            Lpar
            Number 8.4
            Rpar ],
          [ Number 8.0 ]
      )
      TestCaseData(
          [ Function "round"
            Lpar
            Number -3.6
            Rpar ],
          [ Number -4.0 ]
      )
      TestCaseData(
          [ Number 2.0
            Plus
            Function "round"
            Lpar
            Number 8.1
            Rpar ],
          [ Number 10.0 ]
      )
      TestCaseData(
          [ Function "round"
            Lpar
            Number 8.7
            Rpar
            Plus
            Number 5.7 ],
          [ Number 14.7 ]
      )
      TestCaseData(
          [ Function "abs"
            Lpar
            Number 8.4
            Rpar ],
          [ Number 8.4 ]
      )
      TestCaseData(
          [ Function "abs"
            Lpar
            Number -3.6
            Rpar ],
          [ Number 3.6 ]
      )
      TestCaseData(
          [ Number 2.0
            Plus
            Function "abs"
            Lpar
            Number 8.1
            Rpar ],
          [ Number 10.1 ]
      )
      TestCaseData(
          [ Function "gcd"
            Lpar
            Number 1.0
            Comma
            Number 8.0
            Rpar ],
          [ Number 1.0 ]
      )
      TestCaseData(
          [ Function "gcd"
            Lpar
            Number -1.0
            Comma
            Number 14.0
            Rpar ],
          [ Number 1.0 ]
      )
      TestCaseData(
          [ Function "gcd"
            Lpar
            Number 12.0
            Comma
            Number 1.0
            Rpar ],
          [ Number 1.0 ]
      )
      TestCaseData(
          [ Function "gcd"
            Lpar
            Number 1247.0
            Comma
            Number -1.0
            Rpar ],
          [ Number 1.0 ]
      )
      TestCaseData(
          [ Function "gcd"
            Lpar
            Number 29.0
            Comma
            Number 13.0
            Rpar ],
          [ Number 1.0 ]
      )
      TestCaseData(
          [ Function "gcd"
            Lpar
            Number 29.0
            Comma
            Number 12.0
            Rpar ],
          [ Number 1.0 ]
      )
      TestCaseData(
          [ Function "gcd"
            Lpar
            Number 12.0
            Comma
            Number 29.0
            Rpar ],
          [ Number 1.0 ]
      )
      TestCaseData(
          [ Function "gcd"
            Lpar
            Number 27.0
            Comma
            Number 16.0
            Rpar ],
          [ Number 1.0 ]
      )
      TestCaseData(
          [ Function "gcd"
            Lpar
            Number 256.0
            Comma
            Number 248.0
            Rpar ],
          [ Number 8.0 ]
      )
      TestCaseData(
          [ Function "gcd"
            Lpar
            Number 256.0
            Comma
            Number 248.0
            Rpar ],
          [ Number 8.0 ]
      )
      TestCaseData(
          [ Number 12.0
            Plus
            Function "gcd"
            Lpar
            Number 256.0
            Comma
            Number 248.0
            Rpar ],
          [ Number 20.0 ]
      )
      TestCaseData(
          [ Function "gcd"
            Lpar
            Number 256.0
            Comma
            Number 248.0
            Rpar
            Times
            Number 8.0 ],
          [ Number 64.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number 3.0
            Comma
            Number 8.0
            Rpar ],
          [ Number 3.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number 7.0
            Comma
            Number 7.0
            Rpar ],
          [ Number 0.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number 13.0
            Comma
            Number 12.0
            Rpar ],
          [ Number 1.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number -3.0
            Comma
            Number 8.0
            Rpar ],
          [ Number 5.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number -15.0
            Comma
            Number 8.0
            Rpar ],
          [ Number 1.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number 0.0
            Comma
            Number 8.0
            Rpar ],
          [ Number 0.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number 3.0
            Comma
            Number -6.0
            Rpar ],
          [ Number -3.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number 8.0
            Comma
            Number -3.0
            Rpar ],
          [ Number -1.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number -13.0
            Comma
            Number -8.0
            Rpar ],
          [ Number -5.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number -7.0
            Comma
            Number -7.0
            Rpar ],
          [ Number 0.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number -7.0
            Comma
            Number -8.0
            Rpar ],
          [ Number -7.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number -4.0
            Comma
            Number -9.0
            Rpar ],
          [ Number -4.0 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number 0.0
            Comma
            Number -8.0
            Rpar ],
          [ Number 0.0 ]
      )
      TestCaseData(
          [ Number 2.5
            Plus
            Function "mod"
            Lpar
            Number 3.0
            Comma
            Number 8.0
            Rpar ],
          [ Number 5.5 ]
      )
      TestCaseData(
          [ Function "mod"
            Lpar
            Number 3.0
            Comma
            Number 8.0
            Rpar
            Plus
            Number 6.0 ],
          [ Number 9.0 ]
      ) ]

[<TestCaseSource("SystemFunctionCases")>]
let GivenExec_WhenPassedValidSystemFunctionExpression_ReturnCorrectResult
    (
        input: terminal list,
        expected: terminal list
    ) =
    let result, _ = exec env input
    Assert.That(result, Is.EqualTo(expected))

let LogarithmCases =
    [ TestCaseData(
        [ Function "ln"
          Lpar
          Number 2.0
          Rpar ],
        0.693146,
        0.693148
      )
      TestCaseData(
          [ Number 2.0
            Plus
            Function "ln"
            Lpar
            Number 2.0
            Rpar ],
          2.693146,
          2.693148
      )
      TestCaseData(
          [ Function "ln"
            Lpar
            Number 8.0
            Rpar
            Plus
            Number 3.0 ],
          5.079440,
          5.079442
      )
      TestCaseData(
          [ Function "logTen"
            Lpar
            Number 2.0
            Rpar ],
          0.301028,
          0.30103
      )
      TestCaseData(
          [ Function "logTwo"
            Lpar
            Number 2.0
            Rpar ],
          0.999999,
          1.000001
      )
      TestCaseData(
          [ Function "logX"
            Lpar
            Number 8.0
            Comma
            Number 64.0
            Rpar ],
          1.99999,
          2.000002
      )
      TestCaseData(
          [ Function "logX"
            Lpar
            Number 0.0
            Comma
            Number 8.0
            Rpar ],
          -0.99999,
          0.000002
      ) ]

[<TestCaseSource("LogarithmCases")>]
let GivenExec_WhenPassedValidLogarithmExpression_ReturnResultWithinBounds
    (
        input: terminal list,
        lower: float,
        upper: float
    ) =
    let result, _ = exec env input

    match result with
    | [ Number a ] ->
        Assert.That(a, Is.LessThan(upper))
        Assert.That(a, Is.GreaterThan(lower))
    | _ -> ()

let DifferentiateCases =
    [ TestCaseData(
        [ Word "x"
          Plus
          Function "differentiate"
          Lpar
          Number 9.0
          Plus
          Number 17.0
          Rpar ],
        [ Word "x"
          Plus
          Lpar
          Number 0.0
          Rpar ]
      )
      TestCaseData(
          [ Function "differentiate"
            Lpar
            Word "y"
            Plus
            Number 17.0
            Rpar
            Plus
            Word "x" ],
          [ Lpar
            Number 1.0
            Rpar
            Plus
            Word "x" ]
      )
      TestCaseData(
          [ Function "differentiate"
            Lpar
            Word "y"
            Exponent
            Number 2.0
            Rpar ],
          [ Lpar
            Number 2.0
            Times
            Word "y"
            Exponent
            Number 1.0
            Rpar ]
      )
      TestCaseData(
          [ Function "differentiate"
            Lpar
            Word "y"
            Exponent
            Number 2.0
            Comma
            Word "y"
            Assign
            Number 2.0
            Rpar ],
          [ Number 4.0 ]
      )
      TestCaseData(
          [ Number 4.0
            Plus
            Function "differentiate"
            Lpar
            Word "y"
            Exponent
            Number 2.0
            Comma
            Word "y"
            Assign
            Number 2.0
            Rpar ],
          [ Number 8.0 ]
      )
      TestCaseData(
          [ Function "differentiate"
            Lpar
            Word "y"
            Exponent
            Number 2.0
            Comma
            Word "y"
            Assign
            Number 2.0
            Rpar
            Plus
            Number 8.0 ],
          [ Number 12.0 ]
      )
      TestCaseData(
          [ Function "differentiate"
            Lpar
            Function "logX"
            Lpar
            Number 5.0
            Comma
            Word "x"
            Exponent
            Number 12.0
            Rpar
            Comma
            Word "x"
            Assign
            Number 2.0
            Rpar ],
          [ Number 2.316342605425184 ]
      ) ]

[<TestCaseSource("DifferentiateCases")>]
let GivenExec_WhenPassedValidDifferentiation_ReturnCorrectResult (input: terminal list, expected: terminal list) =
    let result, _ = exec Map.empty input
    Assert.That(result, Is.EqualTo(expected))

let GeneralErrorCases =
    [
        TestCaseData([Number 1.0; Number 1.0;])
        TestCaseData([UnaryMinus; Minus;])
        TestCaseData([UnaryMinus; UnaryMinus])
        TestCaseData([Minus; Minus;])
        TestCaseData([Rpar;])
        TestCaseData([Function "ln"; Number 2.0; Times; Number 2.0; Rpar;])
        TestCaseData([Lpar; Number 2.0; Times; Number 2.0;])
        TestCaseData([Lpar; Number 2.0; Times; Number 2.0; Rpar; Rpar;])
        TestCaseData([Lpar;])
        TestCaseData([Function "ln"; Lpar; Number 2.0; Times; Number 2.0;])
        TestCaseData([Lpar; Number 2.0; Times; Number 2.0;])
        TestCaseData([Lpar; Lpar; Number 2.0; Times; Number 2.0; Rpar;])
        TestCaseData([Function "ln"; Lpar; Rpar;])
        TestCaseData([Function "ln"; Lpar; Number 2.0; Times; Number 2.0; Comma; Number 4.0; Rpar;])
        TestCaseData([Function "logX"; Lpar; Number 2.0; Times; Number 2.0; Rpar;])
        TestCaseData([Function "logX"; Lpar; Number 2.0; Times; Number 2.0; Comma; Number 2.0; Times; Number 2.0; Comma; Number 4.0; Rpar;])
        TestCaseData([Function "test"; Lpar; Number 2.0; Times; Number 2.0; Rpar;])
        TestCaseData([Function "differentiate"; Lpar; Function "y"; Lpar; Word "z"; Assign; Number 2.0; Rpar; Rpar;])
        TestCaseData([Function "differentiate"; Lpar; Function "ceil"; Lpar; Number 2.0; Rpar; Rpar;])
        TestCaseData([Function "integrate"; Lpar; Word "x"; Exponent; Number 2.0; Rpar;])
        TestCaseData([Function "integrate"; Lpar; Word "x"; Exponent; Number 2.0; Comma; Number 2.0; Rpar;])
        TestCaseData([Function "integrate"; Lpar; Word "x"; Exponent; Number 2.0; Comma; Number 2.0; Comma; Number 4.0; Rpar; Comma; Number 4.0;])
        TestCaseData([Function "integrate"; Lpar; Word "x"; Exponent; Word "y"; Comma; Number 2.0; Comma; Number 4.0; Rpar;])
        TestCaseData([Function "integrate"; Lpar; Number 2.0; Comma; Number 4.0; Comma; Number 4.0; Rpar;])
        TestCaseData([Function "integrate"; Lpar; Number 2.0; Comma; Number 5.0; Comma; Number 4.0; Rpar;])
        TestCaseData([Function "zeroCrossing"; Lpar; Word "x"; Comma; Number 2.0; Comma; Number 4.0; Rpar;])
        TestCaseData([Function "zeroCrossing"; Lpar; Word "x"; Rpar;])
        TestCaseData([Function "zeroCrossing"; Lpar; Word "x"; Plus; Word "y"; Comma; Number 2.0; Rpar;])
        TestCaseData([Function "zeroCrossing"; Lpar; Word "x"; Comma; Word "x"; Rpar;])
        TestCaseData([Function "sqrt"; Lpar; Plus; Rpar;])
        TestCaseData([Function "y"; Lpar; Word "z"; Assign; Plus; Comma; Word "x"; Assign; Lpar; Rpar;])
        TestCaseData([Word "x"; Assign; Word "y"; Assign; Number 2.0])
        TestCaseData([Word "x"; Assign;])
    ]

let RootErrorCases =
    [
        TestCaseData([Function "xrt"; Lpar; Number -12.0; Comma; Number 4.0; Rpar;])
        TestCaseData([Function "sqrt"; Lpar; Number -2.0; Rpar;])
        TestCaseData([Function "sqrt"; Lpar; UnaryMinus; Number 2.0; Rpar;])
    ]

[<TestCaseSource("GeneralErrorCases")>]
let GivenExec_WhenPassedInvalidStatement_RaiseExecError(input: terminal list) =
    Assert.Throws<ExecError>(fun () -> exec env input |> ignore) |> ignore

[<TestCaseSource("RootErrorCases")>]
let GivenExec_WhenPassedRootWithInvalidArgument_RaiseInvalidArgumentError(input: terminal list) =
    Assert.Throws<InvalidArgumentError>(fun () -> exec env input |> ignore) |> ignore

let IntegrateSuccessCases =
    [
        TestCaseData([Function "integrate"; Lpar; Number 2.0; Comma; Number 2.0; Comma; Number 4.0; Rpar;],3.999,4.001)
        TestCaseData([Function "integrate"; Lpar; Word "x"; Exponent; Number 2.0; Comma; Number 2.0; Comma; Number 4.0; Rpar;],18.666,18.668)
        TestCaseData([Function "integrate"; Lpar; Word "x"; Comma; Number 0.0; Comma; Number 1.0; Rpar;],0.499,0.501)
    ]

[<TestCaseSource(nameof IntegrateSuccessCases)>]
let GivenExec_WhenPassedValidIntegration_ReturnAnswerWithinBounds(input: terminal list, lowerBound: float, upperBound: float) =
    let result, _ = exec Map.empty input
    match result with
    | [Number a] -> Assert.True(lowerBound < a && a < upperBound)
    | _ -> Assert.True(false)

let ZeroCrossingSuccessCases =
    [
        TestCaseData([Function "zeroCrossing"; Lpar; Word "x"; Exponent; Number 2.0; Comma; Number 2.0; Rpar;],-0.01,0.01)
        TestCaseData([Function "zeroCrossing"; Lpar; Word "x"; Exponent; Number 2.0; Comma; Number 40.0; Rpar;],-0.01,0.01)
        TestCaseData([Function "zeroCrossing"; Lpar; Word "x"; Exponent; Number 2.0; Plus; Number 2.0; Times; Word "x"; Comma; Number 2.0; Rpar;],-0.01,0.01)
        TestCaseData([Function "zeroCrossing"; Lpar; Word "x"; Exponent; Number 2.0; Plus; Number 2.0; Times; Word "x"; Comma; Number -32.0; Rpar;],-2.01,-1.99)
    ]

[<TestCaseSource(nameof ZeroCrossingSuccessCases)>]
let GivenExec_WhenPassedValidZeroCrossings_ReturnAnswerWithinBounds(input: terminal list, lowerBound: float, upperBound: float) =
    let result, _ = exec Map.empty input
    match result with
    | [Number a] -> Assert.True(lowerBound < a && a < upperBound)
    | _ -> Assert.True(false)

// Zero Crossing errors into general errors (?)

// map tests

// map errors
