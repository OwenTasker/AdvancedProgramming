/// <summary>
/// Module containing functions for mapping a function onto an array of values.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module Interpreter.Map

open Interpreter.Util
open Interpreter.Exec

/// <summary>Function to compute a linear spaced list of 750 gloats between a lower and upper bound. </summary>
///
/// <param name="lowerBound">The inclusive lowerBound for the array.</param>
/// <param name="upperBound">The inclusive upperBound for the array.</param>
///
/// <returns>A linear space list of 750 floats.</returns>
let private computeXArray lowerBound upperBound =
    let step = (upperBound - lowerBound) / 749.0
    let result = [ lowerBound .. step .. upperBound ]

    if result.Length = 749 then
        result @ [ result.[748] + step ]
    elif result.Length = 750 then
        result
    else
        ExecError "Execution Error: Bounds values gave an invalid x array step."
        |> raise

/// <summary> Function to convert a list of terminals into a list of floats. </summary>
///
/// <param name="terminalList">A list of Number terminals to be converted.</param>
/// <param name="outList">A list of floats as yet converted.</param>
///
/// <returns>A list of floats mirroring the input Number list.</returns>
let rec private terminalListAsFloatList terminalList outList =
    match terminalList with
    | Number a :: tail -> terminalListAsFloatList tail (a :: outList)
    | [] -> List.rev outList
    | _ ->
        ExecError "Execution Error: Reduction of point did not result in Number"
        |> raise

/// <summary>
/// Function to fill an array with the executed value of an expression in a given variable assigned to a given list of
/// values
/// </summary>
///
/// <param name="expression">The expression to be executed.</param>
/// <param name="env">The execution environment for the expression.</param>
/// <param name="variable">The variable contained within the expression.</param>
/// <param name="values">A list of values that the variable is to take.</param>
/// <param name="outList">A list of floats as yet computed by executing the expression with each value</param>
///
/// <returns>A list of floats computed for the expression and the list of values.</returns>
let rec private fillArrayVariable expression (env: Map<string, terminal list>) variable values outList =
    match values with
    | a :: tail ->
        let result, _ =
            exec (env.Add(variable, [ Number a ])) expression

        fillArrayVariable expression env variable tail (outList @ result)
    | [] -> terminalListAsFloatList outList []

/// <summary>
/// Function to fill an array with the executed value of an expression given a list of values.
/// </summary>
///
/// <param name="expression">The expression to be executed.</param>
/// <param name="env">The execution environment for the expression.</param>
/// <param name="values">A list of values.</param>
/// <param name="outList">A list of floats as yet computed by executing the expression</param>
///
/// <returns>A list of floats computed for the expression.</returns>
let rec private fillArray expression (env: Map<string, terminal list>) values outList =
    match values with
    | _ :: tail ->
        let result, _ = exec env expression
        fillArray expression env tail (outList @ result)
    | [] -> terminalListAsFloatList outList []

/// <summary>Function to compute a y array by mapping a statement onto an x array.</summary>
///
/// <param name="statement">The statement to be mapped.</param>
/// <param name="xArray">The values on which the statement is to be mapped.</param>
///
/// <returns>A list of floats representing the mapping of the statement onto the x array.</returns>
let private computeYArray statement xArray=
    match statement with
    | Word a :: Assign :: tail ->
        let env = Map.empty.Add(a, tail)

        if checkUniqueVariables tail Set.empty then
            ExecError "Execution Error: Plotting is currently only supported for two variables"
            |> raise
        else
            let variable = getVariable tail

            try
                if variable <> "!" then
                    fillArrayVariable [ Word a ] env variable xArray []
                else
                    fillArray [ Word a ] env xArray []
            with
            | InvalidArgumentError _
            | ExecError _ ->
                ExecError
                    "Execution Error: The evaluation of this function within the given bounds has undefined values."
                |> raise
    | _ ->
        ExecError "Execution Error: Plotting requires that the first argument be in assignment form."
        |> raise

/// <summary>
/// Function to control the execution of a list of terminals as a computation of a linearly space array followed by
/// given lower and upper bounds followed by a mapping of an expression onto the linearly spaced array
/// </summary>
///
/// <param name="terminals">
/// A list of terminals containing the expression to map, a lower bound, and an upper bound.
/// </param>
///
/// <returns>
/// A thruple containing the linear space array, the mapped array, and the expression that controlled the mapping.
/// </returns>
let internal map terminals =
    match terminals with
    | Function "plot" :: tail ->
        let paramList, remaining = extractParameters tail []

        if remaining.Length > 0 then
            ExecError "Execution Error: Operation cannot be combined with a call to plot"
            |> raise
        elif paramList.Length > 3 then
            ExecError "Execution Error: Too many arguments passed to plot"
            |> raise
        elif paramList.Length = 2 then
            ExecError "Execution Error: Plotting requires 1 or 3 arguments"
            |> raise
        else if paramList.Length = 3 then
            match exec Map.empty paramList.[1] |> fst, exec Map.empty paramList.[2] |> fst with
            | [ Number a ], [ Number b ] ->
                if a < b then
                    let xArray = computeXArray a b
                    let yArray = computeYArray paramList.[0] xArray
                    xArray, yArray, paramList.[0]
                elif b > a then
                    let xArray = computeXArray b a
                    let yArray = computeYArray paramList.[0] xArray
                    xArray, yArray, paramList.[0]
                else
                    let xArray = computeXArray -10.0 10.0
                    let yArray = computeYArray paramList.[0] xArray
                    xArray, yArray, paramList.[0]
            | _ ->
                ExecError "Execution Error: Bounds values must be numbers"
                |> raise
        else
            let xArray = computeXArray -10.0 10.0
            let yArray = computeYArray paramList.[0] xArray
            xArray, yArray, paramList.[0]
    | _ ->
        ExecError "Execution Error: Plot called from non-plot function call"
        |> raise
