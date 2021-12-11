module Interpreter.Map

open Interpreter.Util
open Interpreter.Exec

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

let rec private terminalListAsFloatList terminalList outList =
    match terminalList with
    | Number a :: tail -> terminalListAsFloatList tail (a :: outList)
    | [] -> List.rev outList
    | _ ->
        ExecError "Execution Error: Reduction of point did not result in Number"
        |> raise

let rec private fillArrayVariable
    expression
    (env: Map<string, terminal list>)
    variable
    values
    (outList: terminal list)
    : float list =
    match values with
    | a :: tail ->
        let result, _ =
            exec (env.Add(variable, [ Number a ])) expression

        fillArrayVariable expression env variable tail (outList @ result)
    | [] -> terminalListAsFloatList outList []

let rec private fillArray expression (env: Map<string, terminal list>) values outList =
    match values with
    | _ :: tail ->
        let result, _ = exec env expression
        fillArray expression env tail (outList @ result)
    | [] -> terminalListAsFloatList outList []

let private computeYArray statement (xArray: float list) : float list =
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


let internal map terminals =
    match terminals with
    | Function "plot" :: tail ->
        let paramList, remaining = extractParameters tail [] Map.empty

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
