module Interpreter.Tests.DifferentiateTests

open NUnit.Framework
open Interpreter.Differentiate
open Interpreter.Util

let DifferentiateCases =
    [ TestCaseData([ Number 2.0 ], [ Number 0.0 ])
      TestCaseData([ Word "x" ], [ Number 1.0 ])
      TestCaseData([ Word "x"; Plus; Word "x" ], [ Number 2.0 ])
      TestCaseData([ Word "x"; Times; Word "x" ], [ Word "x"; Plus; Word "x" ])
      TestCaseData(
          [ Word "x"; Exponent; Number 2.0 ],
          [ Number 2.0
            Times
            Word "x"
            Exponent
            Number 1.0 ]
      )
      TestCaseData(
          [ Number 2.0; Exponent; Word "x" ],
          [ Function "ln"
            Lpar
            Number 2.0
            Rpar
            Times
            Number 2.0
            Exponent
            Word "x" ]
      ) ]

let SimpleSingleCases =
    [ TestCaseData([ Number 2.0 ], [ Number 0.0 ])
      TestCaseData([ Number 0.0 ], [ Number 0.0 ])
      TestCaseData([ Number -100.0, [ Number 0.0 ] ])
      TestCaseData([ Word "x" ], [ Number 1.0 ])
      TestCaseData([ Word "!" ], [ Number 1.0 ]) ]

let SimplePlusCases =
    [ TestCaseData([ Number 2.0; Plus; Number 4.0 ], [ Number 0.0 ])
      TestCaseData([ Number 2.0; Plus; Word "x" ], [ Number 1.0 ])
      TestCaseData([ Word "^"; Plus; Number 12.0 ], [ Number 1.0 ])
      TestCaseData([ Word "^"; Plus; Word "^" ], [ Number 2.0 ]) ]

let SimpleMinusCases =
    [ TestCaseData([ Number 2.0; Minus; Number 4.0 ], [ Number 0.0 ])
      TestCaseData([ Number 2.0; Minus; Word "x" ], [ Number -1.0 ])
      TestCaseData([ Word "^"; Minus; Number 12.0 ], [ Number 1.0 ])
      TestCaseData([ Word "^"; Minus; Word "^" ], [ Number 0.0 ]) ]

let SimpleTimesCases =
    [ TestCaseData([ Number 2.0; Times; Number 4.0 ], [ Number 0.0 ])
      TestCaseData([ Number 2.0; Times; Word "x" ], [ Number 2.0 ])
      TestCaseData([ Word "^"; Times; Number 12.0 ], [ Number 12.0 ])
      TestCaseData([ Word "^"; Times; Word "^" ], [ Word "^"; Plus; Word "^" ]) ]

let SimpleDivideCases =
    [ TestCaseData([ Number 2.0; Divide; Number 4.0 ], [ Number 0.0 ])
      TestCaseData(
          [ Number 2.0; Divide; Word "x" ],
          [ UnaryMinus
            Number 2.0
            Divide
            Word "x"
            Exponent
            Number 2.0 ]
      )
      TestCaseData([ Word "^"; Divide; Number 12.0 ], [ Number(1.0 / 12.0) ])
      TestCaseData([ Word "^"; Divide; Word "^" ], [ Number 0.0 ]) ]

let SimpleExponentCases =
    [ TestCaseData([ Number 2.0; Exponent; Number 4.0 ], [ Number 0.0 ])
      TestCaseData(
          [ Number 2.0; Exponent; Word "x" ],
          [ Function "ln"
            Lpar
            Number 2.0
            Rpar
            Times
            Number 2.0
            Exponent
            Word "x" ]
      )
      TestCaseData(
          [ Word "^"; Exponent; Number 12.0 ],
          [ Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0 ]
      )
      TestCaseData(
          [ Word "^"; Exponent; Word "^" ],
          [ Word "^"
            Exponent
            Word "^"
            Times
            Lpar
            Function "ln"
            Lpar
            Word "^"
            Rpar
            Plus
            Number 1.0
            Rpar ]
      ) ]

let ExpressionPlusCases =
    [ TestCaseData(
        [ Number 6.0
          Plus
          Lpar
          Word "^"
          Exponent
          Number 12.0
          Rpar ],
        [ Number 12.0
          Times
          Word "^"
          Exponent
          Number 11.0 ]
      )
      TestCaseData(
          [ Word "^"
            Plus
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar ],
          [ Number 1.0
            Plus
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Plus
            Number 6.0 ],
          [ Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Plus
            Word "^" ],
          [ Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Plus
            Number 1.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Plus
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar ],
          [ Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Plus
            Number 11.0
            Times
            Word "^"
            Exponent
            Number 10.0 ]
      ) ]

let ExpressionMinusCases =
    [ TestCaseData(
        [ Number 6.0
          Minus
          Lpar
          Word "^"
          Exponent
          Number 12.0
          Rpar ],
        [ UnaryMinus
          Number 12.0
          Times
          Word "^"
          Exponent
          Number 11.0 ]
      )
      TestCaseData(
          [ Word "^"
            Minus
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar ],
          [ Number 1.0
            Minus
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Minus
            Number 6.0 ],
          [ Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Minus
            Word "^" ],
          [ Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Minus
            Number 1.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Minus
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar ],
          [ Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Minus
            Number 11.0
            Times
            Word "^"
            Exponent
            Number 10.0 ]
      ) ]

let ExpressionTimesCases =
    [ TestCaseData(
        [ Number 6.0
          Times
          Lpar
          Word "^"
          Exponent
          Number 12.0
          Rpar ],
        [ Number 6.0
          Times
          Lpar
          Number 12.0
          Times
          Word "^"
          Exponent
          Number 11.0
          Rpar ]
      )
      TestCaseData(
          [ Word "^"
            Times
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar ],
          [ Word "^"
            Times
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Plus
            Word "^"
            Exponent
            Number 12.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Times
            Number 6.0 ],
          [ Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Number 6.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Times
            Word "^" ],
          [ Word "^"
            Exponent
            Number 12.0
            Plus
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Word "^" ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Times
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar ],
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Times
            Lpar
            Number 11.0
            Times
            Word "^"
            Exponent
            Number 10.0
            Rpar
            Plus
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar ]
      ) ]

let ExpressionDivideCases =
    [ TestCaseData(
        [ Number 6.0
          Divide
          Lpar
          Word "^"
          Exponent
          Number 12.0
          Rpar ],
        [ Lpar
          UnaryMinus
          Number 6.0
          Times
          Lpar
          Number 12.0
          Times
          Word "^"
          Exponent
          Number 11.0
          Rpar
          Rpar
          Divide
          Lpar
          Word "^"
          Exponent
          Number 12.0
          Rpar
          Exponent
          Number 2.0 ]
      )
      TestCaseData(
          [ Word "^"
            Divide
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar ],
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Minus
            Word "^"
            Times
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Rpar
            Divide
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Exponent
            Number 2.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Divide
            Number 6.0 ],
          [ Lpar
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Number 6.0
            Rpar
            Divide
            Number 36.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Divide
            Word "^" ],
          [ Lpar
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Word "^"
            Minus
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Rpar
            Divide
            Word "^"
            Exponent
            Number 2.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Divide
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar ],
          [ Lpar
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Minus
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Times
            Lpar
            Number 11.0
            Times
            Word "^"
            Exponent
            Number 10.0
            Rpar
            Rpar
            Divide
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Exponent
            Number 2.0 ]
      ) ]

let ExpressionExponentCases =
    [ TestCaseData(
        [ Number 6.0
          Exponent
          Lpar
          Word "^"
          Exponent
          Number 12.0
          Rpar ],
        [ Function "ln"
          Lpar
          Number 6.0
          Rpar
          Times
          Lpar
          Number 12.0
          Times
          Word "^"
          Exponent
          Number 11.0
          Rpar
          Times
          Number 6.0
          Exponent
          Lpar
          Word "^"
          Exponent
          Number 12.0
          Rpar ]
      )
      TestCaseData(
          [ Word "^"
            Exponent
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar ],
          [ Word "^"
            Exponent
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Times
            Lpar
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Function "ln"
            Lpar
            Word "^"
            Rpar
            Plus
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Divide
            Word "^"
            Rpar ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Exponent
            Number 6.0 ],
          [ Number 6.0
            Times
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Exponent
            Number 5.0 ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Exponent
            Word "^" ],
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Exponent
            Word "^"
            Times
            Lpar
            Function "ln"
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Plus
            Lpar
            Word "^"
            Times
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Divide
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Rpar ]
      )
      TestCaseData(
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Exponent
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar ],
          [ Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Exponent
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Lpar
            Lpar
            Number 11.0
            Times
            Word "^"
            Exponent
            Number 10.0
            Rpar
            Times
            Function "ln"
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Plus
            Lpar
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Rpar
            Divide
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Rpar ]
      ) ]

let FunctionCases =
    [ TestCaseData(
        [ Function "ln"
          Lpar
          Word "^"
          Exponent
          Number 12.0
          Rpar ],
        [ Lpar
          Number 12.0
          Times
          Word "^"
          Exponent
          Number 11.0
          Rpar
          Times
          Lpar
          Number 1.0
          Divide
          Lpar
          Word "^"
          Exponent
          Number 12.0
          Rpar
          Rpar ]
      )
      TestCaseData(
          [ Function "abs"
            Lpar
            Number 2.0
            Times
            Word "x"
            Plus
            Number 1.0
            Rpar ],
          [ Lpar
            Lpar
            Number 2.0
            Rpar
            Times
            Lpar
            Number 2.0
            Times
            Word "x"
            Plus
            Number 1.0
            Rpar
            Divide
            Function "abs"
            Lpar
            Number 2.0
            Times
            Word "x"
            Plus
            Number 1.0
            Rpar
            Rpar ]
      ) ]

let UnaryCases =
    [ TestCaseData([ UnaryMinus; Number 12.0 ], [ Number 0.0 ])
      TestCaseData([ UnaryPlus; Number 12.0 ], [ Number 0.0 ])
      TestCaseData([ UnaryMinus; Word "x" ], [ UnaryMinus; Number 1.0 ])
      TestCaseData([ UnaryPlus; Word "x" ], [ Number 1.0 ])
      TestCaseData(
          [ UnaryMinus
            Lpar
            Word "x"
            Exponent
            Number 2.0
            Rpar ],
          [ UnaryMinus
            Lpar
            Number 2.0
            Times
            Word "x"
            Exponent
            Number 1.0
            Rpar ]
      )
      TestCaseData(
          [ UnaryPlus
            Lpar
            Word "x"
            Exponent
            Number 2.0
            Rpar ],
          [ Number 2.0
            Times
            Word "x"
            Exponent
            Number 1.0 ]
      ) ]

let ComplexCases =
    [ TestCaseData(
          [ Lpar
            Lpar
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Minus
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Times
            Lpar
            Lpar
            Number 0.0
            Rpar
            Times
            Lpar
            Number 1.0
            Divide
            Lpar
            Number 5.0
            Rpar
            Rpar
            Rpar
            Rpar
            Divide
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Exponent
            Number 2.0
            Rpar
            Times
            Lpar
            Number 1.0
            Divide
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Divide
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar ],
          [ Lpar
            Lpar
            Lpar
            Number 12.0
            Times
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Rpar
            Times
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Minus
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Times
            Number 0.0
            Rpar
            Divide
            Lpar
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Exponent
            Number 2.0
            Rpar
            Rpar
            Times
            Lpar
            Lpar
            Lpar
            Lpar
            UnaryMinus
            Number 1.0
            Times
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Rpar
            Divide
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Exponent
            Number 2.0
            Rpar
            Times
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Minus
            Lpar
            Number 1.0
            Divide
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Rpar
            Times
            Lpar
            Lpar
            Number 0.0
            Rpar
            Times
            Lpar
            Number 1.0
            Divide
            Lpar
            Number 5.0
            Rpar
            Rpar
            Rpar
            Rpar
            Divide
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Exponent
            Number 2.0
            Rpar
            Plus
            Lpar
            Lpar
            Lpar
            Lpar
            Number 12.0
            Times
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Rpar
            Times
            Lpar
            Lpar
            Number 0.0
            Rpar
            Times
            Lpar
            Number 1.0
            Divide
            Lpar
            Number 5.0
            Rpar
            Rpar
            Rpar
            Plus
            Lpar
            Number 12.
            Times
            Lpar
            Number 11.0
            Times
            Word "^"
            Exponent
            Number 10.0
            Rpar
            Rpar
            Times
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Minus
            Lpar
            Number 12.0
            Times
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Times
            Number 0.0
            Rpar
            Times
            Lpar
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Exponent
            Number 2.0
            Rpar
            Minus
            Lpar
            Lpar
            Number 12.0
            Times
            Lpar
            Word "^"
            Exponent
            Number 11.0
            Rpar
            Rpar
            Times
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Minus
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Times
            Number 0.0
            Rpar
            Times
            Lpar
            Number 2.0
            Times
            Lpar
            Lpar
            Number 0.0
            Rpar
            Times
            Lpar
            Number 1.0
            Divide
            Lpar
            Number 5.0
            Rpar
            Rpar
            Rpar
            Times
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Exponent
            Number 1.0
            Rpar
            Rpar
            Divide
            Lpar
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Exponent
            Number 2.0
            Rpar
            Exponent
            Number 2.0
            Rpar
            Times
            Lpar
            Lpar
            Number 1.0
            Divide
            Lpar
            Word "^"
            Exponent
            Number 12.0
            Rpar
            Rpar
            Divide
            Lpar
            Function "ln"
            Lpar
            Number 5.0
            Rpar
            Rpar
            Rpar ]
      ) ]

[<TestCaseSource(nameof DifferentiateCases)>]
[<TestCaseSource(nameof SimplePlusCases)>]
[<TestCaseSource(nameof SimpleMinusCases)>]
[<TestCaseSource(nameof SimpleTimesCases)>]
[<TestCaseSource(nameof SimpleDivideCases)>]
[<TestCaseSource(nameof SimpleExponentCases)>]
[<TestCaseSource(nameof ExpressionPlusCases)>]
[<TestCaseSource(nameof ExpressionMinusCases)>]
[<TestCaseSource(nameof ExpressionTimesCases)>]
[<TestCaseSource(nameof ExpressionDivideCases)>]
[<TestCaseSource(nameof ExpressionExponentCases)>]
[<TestCaseSource(nameof FunctionCases)>]
[<TestCaseSource(nameof UnaryCases)>]
[<TestCaseSource(nameof ComplexCases)>]
let GivenDifferentiate_WhenPassedValidExpression_ReturnFirstDerivative
    (
        expression: terminal list,
        expected: terminal list
    ) =
    let result = differentiate expression
    Assert.That(result, Is.EqualTo(expected))


let ErrorCases =
    [ TestCaseData([ Number 1.0; Plus; Plus ])
      TestCaseData([ Number 1.0; Plus; Plus; Number 1.0 ])
      TestCaseData([ Number 1.0; Number 1.0 ])
      TestCaseData(
          [ Lpar
            Number 1.0
            Plus
            Plus
            Number 1.0 ]
      )
      TestCaseData(
          [ Number 1.0
            Plus
            Plus
            Number 1.0
            Rpar ]
      )
      TestCaseData(
          [ Function "lm"
            Lpar
            Number 1.0
            Rpar ]
      )
      TestCaseData(
          [ Word "x"
            Plus
            Word "y" ]
      ) ]

[<TestCaseSource(nameof ErrorCases)>]
let GivenDifferentiate_WhenPassedInvalidExpression_RaiseExecError (expression: terminal list) =
    Assert.Throws<ExecError>(fun () -> differentiate expression |> ignore)
    |> ignore
