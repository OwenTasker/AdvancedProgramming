/// <summary>
/// Module containing tests for the functions defined in Interpreter.MathematicalFunctions.fs.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter.Tests</summary>
/// </namespacedoc>
module Interpreter.Tests.MathematicalFunctionsTests

open Interpreter.Util
open NUnit.Framework
open Interpreter.MathematicalFunctions

//Valid Log Inputs and Outputs
let LogEValidInputsOutputs =
    [
    TestCaseData(0.01, Number -4.605170)
    TestCaseData(0.25, Number -1.386294)
    TestCaseData(0.5, Number -0.693147)
    TestCaseData(0.6, Number -0.510825)
    TestCaseData(0.75, Number -0.287682)
    TestCaseData(0.9, Number -0.105360)
    TestCaseData(1.0, Number 0.0)
    TestCaseData(1.5, Number 0.405465)
    TestCaseData(2.0, Number 0.693147)
    TestCaseData(2.718281, Number 1.0)
    TestCaseData(3.0, Number 1.098612)
    TestCaseData(5.0, Number 1.609437)
    TestCaseData(10.0, Number 2.302585)
    TestCaseData(100.0, Number 4.605170)
    ]

let Log2ValidInputOutputs =
    [
    TestCaseData(0.25, -2.0)
    TestCaseData(0.5, -1)
    TestCaseData(0.6, -0.736965)
    TestCaseData(0.75, -0.415037)
    TestCaseData(0.9, -0.152003)
    TestCaseData(1.0, 0.0)
    TestCaseData(1.5, 0.584962)
    TestCaseData(2.0, 1.0)
    TestCaseData(2.718281, 1.442694)
    TestCaseData(3.0, 1.584962)
    TestCaseData(5.0, 2.321928)
    TestCaseData(10.0, 3.321928)
    TestCaseData(100.0, 6.643856)
    ]

let Log10ValidInputOutputs =
    [
    TestCaseData(0.25, -0.602059)
    TestCaseData(0.5, -0.301029)
    TestCaseData(0.6, -0.221848)
    TestCaseData(0.75, -0.124938)
    TestCaseData(0.9, -0.045757)
    TestCaseData(1.0, 0.0)
    TestCaseData(1.5, 0.176091)
    TestCaseData(2.0, 0.301029)
    TestCaseData(2.718281, 0.434294)
    TestCaseData(3.0, 0.477121)
    TestCaseData(5.0, 0.698970)
    TestCaseData(10.0, 1.0)
    TestCaseData(100.0, 2.0)
    ]

let LogXValidInputOutputs =
    [
    TestCaseData(0.0, 1.0, 0.0)
    TestCaseData(0.0, 5.0, 0.0)

    TestCaseData(3.0, 0.25, -1.261859)
    TestCaseData(3.0, 0.5, -0.630929)
    TestCaseData(3.0, 0.6, -0.464973)
    TestCaseData(3.0, 0.75, -0.261859)
    TestCaseData(3.0, 0.9, -0.095903)
    TestCaseData(3.0, 1.0, 0.0)
    TestCaseData(3.0, 1.5, 0.369070)
    TestCaseData(3.0, 2.0, 0.630929)
    TestCaseData(3.0, 2.718281, 0.910239)
    TestCaseData(3.0, 3.0, 1.0)
    TestCaseData(3.0, 5.0, 1.464973)
    TestCaseData(3.0, 10.0, 2.095903)
    TestCaseData(3.0, 100.0, 4.191806)

    TestCaseData(5, 0.25, -0.861353)
    TestCaseData(5, 0.5, -0.430676)
    TestCaseData(5, 0.6, -0.317393)
    TestCaseData(5, 0.75, -0.178746)
    TestCaseData(5, 0.9, -0.065464)
    TestCaseData(5, 1.0, 0.0)
    TestCaseData(5, 1.5, 0.251929)
    TestCaseData(5, 2.0, 0.430676)
    TestCaseData(5, 2.718281, 0.621334)
    TestCaseData(5, 3.0, 0.682606)
    TestCaseData(5, 5.0, 1.0)
    TestCaseData(5, 10.0, 1.430676)
    TestCaseData(5, 100.0, 2.861353)
    ]

//Invalid Log Inputs
let GenericInvalidLogInputs =
    [
        TestCaseData(0.0)
        TestCaseData(-0.1)
        TestCaseData(-1.0)
        TestCaseData(-5.0)
    ]
    
let LogXInvalidInputs =
    [
    TestCaseData(3.0, 0.0)
    TestCaseData(3.0, -0.1)
    TestCaseData(3.0, -1.0)
    TestCaseData(3.0, -5.0)

    TestCaseData(5.0, 0.0)
    TestCaseData(5.0, -0.1)
    TestCaseData(5.0, -1.0)
    TestCaseData(5.0, -5.0)

    TestCaseData(7.0, 0.0)
    TestCaseData(7.0, -0.1)
    TestCaseData(7.0, -1.0)
    TestCaseData(7.0, -5.0)

    TestCaseData(0.0, 0.0)
    TestCaseData(-1.0, 0.0)
    TestCaseData(-5.0, -0.1)
    TestCaseData(-1.0, -5.0)
    ]

[<TestCaseSource(nameof Log2ValidInputOutputs)>]
let givenLog2_ProvidedValidInput_ReturnCorrectApproximation input output =
    let logRes = (Log2 input) |> terminalToNum
    let a = logRes > (output - 0.000001)
    let b = logRes < (output + 0.000001)
    Assert.True(a && b)

[<TestCaseSource(nameof Log10ValidInputOutputs)>]
let givenLog10_ProvidedValidInput_ReturnCorrectApproximation input output =
    let logRes = (Log10 input) |> terminalToNum
    let a = logRes > (output-0.000001)
    let b = logRes < (output+0.000001)
    Assert.True(a && b)

[<TestCaseSource(nameof LogXValidInputOutputs)>]
let givenLogX_ProvidedValidInput_ReturnCorrectApproximation logBase input output =
    let logRes = LogX logBase input
    let a = logRes > (output - 0.000001 |> Number)
    let b = logRes < (output + 0.000001 |> Number)
    Assert.True(a && b)
    
[<TestCaseSource(nameof LogEValidInputsOutputs)>]
let givenLogETerminal_ProvidedValidInputsOutputs_ReturnCorrectApproximation input output =
    let logRes = LogETerminal input
    let outputNumRep = terminalToNum output 
    let a = logRes > (outputNumRep - 0.000001 |> Number)
    let b = logRes < (outputNumRep + 0.000001 |> Number)
    Assert.True(a && b)    

[<TestCaseSource(nameof GenericInvalidLogInputs)>]
let givenLog10_ProvidedInvalidInputs_ThrowInvalidArgumentError input =
    Assert.Throws<InvalidArgumentError>(fun () -> Log10 input |> ignore) |> ignore

[<TestCaseSource(nameof GenericInvalidLogInputs)>]
let givenLog2_ProvidedInvalidInputs_ThrowInvalidArgumentError input =
    Assert.Throws<InvalidArgumentError>(fun () -> Log2 input |> ignore) |> ignore

[<TestCaseSource(nameof LogXInvalidInputs)>]
let givenLogX_ProvidedInvalidInputs_ThrowInvalidArgumentError newBase input =
    Assert.Throws<InvalidArgumentError>(fun () -> (LogX newBase input) |> ignore) |> ignore
    
[<TestCaseSource(nameof GenericInvalidLogInputs)>]
let givenLogETerminal_ProvidedInvalidInputs_ThrowInvalidArgumentError input =
    Assert.Throws<InvalidArgumentError>(fun () -> (LogETerminal input) |> ignore) |> ignore

//RootToTerminal Test Cases
let rootToTerminalValidInputs =
    [
        //order is -- Xth root of Y
        //[Number 1.0], 2.0 means the 1st root of 2
        TestCaseData([Number 1.0], [Number 2.0], [Lpar; Lpar;Number 1.0;Rpar; Exponent; Lpar; Number 1.0; Divide; Number 2.0; Rpar; Rpar])
        TestCaseData([Number 10.0], [Number 2.0],[Lpar; Lpar; Number 10.0; Rpar; Exponent; Lpar; Number 1.0; Divide; Number 2.0; Rpar; Rpar])
        TestCaseData([Number 10.0; Divide; Number 3.0], [Number 2.0], [Lpar; Lpar; Number 10.0; Divide; Number 3.0; Rpar; Exponent; Lpar; Number 1.0; Divide; Number 2.0; Rpar; Rpar])
        TestCaseData([Word "x"; Plus; Number 3.0], [Number 3.0], [Lpar; Lpar; Word "x"; Plus; Number 3.0; Rpar; Exponent; Lpar; Number 1.0; Divide; Number 3.0; Rpar; Rpar])
        TestCaseData([Number 2.0], [Number 2.0], [Lpar; Lpar; Number 2.0; Rpar; Exponent; Lpar; Number 1.0; Divide; Number 2.0; Rpar; Rpar])
    ]

let rootToTerminalInvalidInputs =
    [
        //Cannot test first argument/bases here outside of the empty case, this needs to be done via integration testing
        TestCaseData(([]:terminal list), [Number 1.0])
        TestCaseData([Number 2.0], ([]:terminal list))
    ]

[<TestCaseSource(nameof rootToTerminalValidInputs)>]
let givenRootToTerminals_ProvidedValidInput_ReturnCorrectValues inputList denominator output =
    let rootToTerminalRes = RootToTerminals inputList denominator
    Assert.That(rootToTerminalRes, Is.EqualTo(output))

[<TestCaseSource(nameof rootToTerminalInvalidInputs)>]
let givenRootToTerminals_ProvidedInvalidInputs_ThrowInvalidArgumentError newBase input =
    Assert.Throws<InvalidArgumentError>(fun () -> (RootToTerminals newBase input) |> ignore) |> ignore

//FloorToTerminal Test Cases
let floorToTerminalInputsOutputs =
    [
        TestCaseData(0.0, Number 0.0)
        TestCaseData(1.1, Number 1.0)
        TestCaseData(1.9, Number 1.0)
        TestCaseData(-0.0, Number 0.0)
        TestCaseData(-0.1, Number -1.0)
        TestCaseData(-0.9, Number -1.0)
        TestCaseData(-5.0, Number -5.0)
        TestCaseData(-4.5, Number -5.0)
    ]

[<TestCaseSource(nameof floorToTerminalInputsOutputs)>]
let givenFloorToTerminal_ProvidedValidInput_ReturnCorrectResult input output =
    Assert.That((FloorToTerminal input), Is.EqualTo(output))

//CeilToTerminal Test Cases
let ceilToTerminalInputsOutputs =
    [
        TestCaseData(0.0, Number 0.0)
        TestCaseData(1.1, Number 2.0)
        TestCaseData(1.9, Number 2.0)
        TestCaseData(-0.0, Number 0.0)
        TestCaseData(-0.1, Number 0.0)
        TestCaseData(-0.9, Number 0.0)
        TestCaseData(-5.0, Number -5.0)
        TestCaseData(-4.5, Number -4.0)
    ]

[<TestCaseSource(nameof ceilToTerminalInputsOutputs)>]
let givenCeilToTerminal_ProvidedValidInput_ReturnCorrectResult input output =
    Assert.That((CeilToTerminal input), Is.EqualTo(output))

//RoundToTerminal Test Cases
let roundNumInputsOutputs =
    [
        TestCaseData(0.0,Number 0.0)
        TestCaseData(0.5,Number 1.0)
        TestCaseData(0.4,Number 0.0)
        TestCaseData(1.1,Number 1.0)
        TestCaseData(-0.4,Number 0.0)
        TestCaseData(-0.5,Number -1.0)
        TestCaseData(-4.5,Number -5.0)
        TestCaseData(-4.4,Number -4.0)
        TestCaseData(4.4,Number 4.0)
        TestCaseData(4.5,Number 5.0)
    ]

[<TestCaseSource(nameof roundNumInputsOutputs)>]
let givenRoundNum_ProvidedValidInput_ReturnCorrectResult input output =
    Assert.That((RoundNum input), Is.EqualTo(output))

//AbsVal Test Cases
let absValInputsOutputs =
    [
        TestCaseData(0.0, Number 0.0)
        TestCaseData(-0.0, Number 0.0)
        TestCaseData(-5.0, Number 5.0)
        TestCaseData(5.0, Number 5.0)
        TestCaseData(-2.2, Number 2.2)
    ]

[<TestCaseSource(nameof absValInputsOutputs)>]
let givenAbsVal_ProvidedValidInput_ReturnCorrectResult input output =
    Assert.That((AbsVal input), Is.EqualTo(output))

//getGCD Test Cases
let getGCDWrapperValidInputsOutputs =
    [
        TestCaseData(0.0, 0.0, Number 0.0)
        TestCaseData(0.0, 2.0, Number 0.0)
        TestCaseData(2.0, 0.0, Number 0.0)
        TestCaseData(2.0, 2.0, Number 2.0)
        TestCaseData(12.0, 30.0, Number 6.0)
        TestCaseData(30.0, 105.0, Number 15.0)
    ]

let getGCDWrapperInvalidInputs =
    [
        TestCaseData(4.5, 20.2)
        TestCaseData(-4.4, 20.2)
        TestCaseData(-4.4, -20.2)
        TestCaseData(4.4, -20.2)
    ]

[<TestCaseSource(nameof getGCDWrapperValidInputsOutputs)>]
let givenGetGCDWrapper_ProvidedValidInputs_ReturnCorrectResult input1 input2 output =
    Assert.That(getGCDWrapper input1 input2, Is.EqualTo(output))

[<TestCaseSource(nameof getGCDWrapperInvalidInputs)>]
let givenGetGCDWrapper_ProvidedInvalidInputs_ThrowInvalidArgumentError input1 input2 =
    Assert.Throws<InvalidArgumentError>(fun () -> (getGCDWrapper input1 input2) |> ignore) |> ignore

//Modulo Test Cases
let moduloValidInputsOutputs =
    [
        //0 in left vals
        TestCaseData(0.0, 1.0, Number 0.0)
        TestCaseData(0.0, 5.0, Number 0.0)
        TestCaseData(0.0, 10.0, Number 0.0)
        TestCaseData(0.0, 50.0, Number 0.0)

        //Positive left vals, Positive right vals
        TestCaseData(3.0, 2.0, Number 1.0)
        TestCaseData(20.0, 7.0, Number 6.0)
        TestCaseData(15.0, 5.0, Number 0.0)
        TestCaseData(3.0, 3.0, Number 0.0)
        
        //Positive left vals, Negative right vals
        TestCaseData(3.0, -2.0, Number -1.0)
        TestCaseData(7.0, -7.0, Number 0.0)
        TestCaseData(4.0, -8.0, Number -4.0)
        TestCaseData(4.0, -4.0, Number 0.0)
        
        //Negative left vals, Negative right vals
        TestCaseData(-4.0, -8.0, Number -4.0)
        TestCaseData(-8.0, -4.0, Number 0.0)
        TestCaseData(-4.0, -4.0, Number 0.0)
        TestCaseData(-2.0, -8.0, Number -2.0)
        
        //Positive Non-Whole number left vals, positive right vals
        TestCaseData(2.2, 1.0, Number 0.2)
                
    ]

let moduloInvalidInputs =
    [
        //Zero Right Vals
        TestCaseData(1.0, 0.0)
        TestCaseData(15.0, 0.0)
        TestCaseData(0.0, 0.0)
        TestCaseData(-1.0, 0.0)
        TestCaseData(-15.0, 0.0)
    ]


[<TestCaseSource(nameof moduloValidInputsOutputs)>]
let givenModuloCalc_ProvidedValidInputs_ReturnCorrectOutput input1 input2 output =
    let calc = moduloCalc input1 input2 |> terminalToNum
    let outputAsNum = terminalToNum output
    let calcGROutComp = calc > (outputAsNum - 0.000001)
    let calcLSOutComp = calc < (outputAsNum + 0.000001)
    let isCalcWithinRange = calcGROutComp && calcLSOutComp
    
    Assert.True(isCalcWithinRange)

[<TestCaseSource(nameof moduloInvalidInputs)>]
let givenModuloCalc_ProvidedInvalidInputs_ThrowInvalidArgumentError input1 input2 =
    Assert.Throws<InvalidArgumentError>(fun () -> (moduloCalc input1 input2) |> ignore) |> ignore
    
//Pseudorandom testing
let pseudoRandomValidInputs =
    [
        TestCaseData(-20.0, -10.0)
        TestCaseData(0.0, 100.0)
        TestCaseData(-100.0, 0.0)
        TestCaseData(-20.0, 20.0)
    ]

let pseudoRandomInvalidInputs =
    [
        // Arg2 Less Than Arg1
        TestCaseData(10.0, 8.0)
        TestCaseData(5.0, 2.0)
        TestCaseData(-13.0, -15.0)
        
        // Arg1 and Arg2 equal
        TestCaseData(10.0, 10.0)
        TestCaseData(2.0, 2.0)
        TestCaseData(-13.0, -13.0)
        
        //Either args are not whole numbers
        TestCaseData(2.5, 5.0)
        TestCaseData(1.0, 7.5)
        TestCaseData(5.2, 15.6)
    ]
    
[<TestCaseSource(nameof pseudoRandomValidInputs)>]
let givenPseudoRandom_ProvidedValidInputs_DoNotThrowExceptions input1 input2 =
    Assert.DoesNotThrow(fun () -> (pseudoRandom input1 input2 |> ignore))
    
[<TestCaseSource(nameof pseudoRandomInvalidInputs)>]
let givenPseudoRandom_ProvidedInvalidInputs_ThrowInvalidArgumentError input1 input2 =
    Assert.Throws<InvalidArgumentError>(fun () -> (pseudoRandom input1 input2) |> ignore) |> ignore