/// <summary>
/// Module containing functions for execution of commands passed to the MyMathsPal Interpreter.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module internal Interpreter.Exec

open Interpreter.Util
open Interpreter.Differentiate
open Interpreter.MathematicalFunctions

// http://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/

/// <summary>
/// Performs a binary operation given a terminal representing plus, minus, times, divide, or exponent
/// and two operands as Number terminals.
/// </summary>
///
/// <param name="operator">A terminal representing an operator.</param>
/// <param name="op1">A Number terminal for the left side of the operation.</param>
/// <param name="op2">A Number terminal for the right side of the operation.</param>
///
/// <returns>A Number containing the result of the operation.</returns>
let private performBinaryOperation operator op1 op2 =
    match operator with
    | Plus -> Number(op1 + op2)
    | Minus -> Number(op1 - op2)
    | Times -> Number(op1 * op2)
    | Divide ->
        match op2 with
        | 0.0 ->
            CalculateError "Calculate Error: Attempting to divide by zero, this operation is undefined."
            |> raise
        | _ -> Number(op1 / op2)
    | Exponent ->
        if op1 < 0.0 && 0.0 < op2 && op2 < 1.0 then
            Number(-((-op1) ** op2))
        else
            Number(op1 ** op2)
    | _ ->
        CalculateError "Calculate Error: Invalid operator passed."
        |> raise

/// <summary>
/// Performs a unary arithmetic operation given terminals representing a unary operator and an operand.
/// </summary>
///
/// <param name="operator">A terminal representing an operator.</param>
/// <param name="operand">A Number terminal containing the operand.</param>
///
/// <returns>A Number containing the result of the operation.</returns>
let private performUnaryOperation operator operand =
    match operator with
    | UnaryMinus -> Number -operand
    | UnaryPlus -> Number operand
    | _ ->
        UnaryError "Unary Error: Invalid operator passed."
        |> raise

/// <summary>
/// Reads the top of the operator stack and passes said operator and necessary operands from the number stack to
/// the correct handling method. Prepends the result to the number stack.
/// </summary>
///
/// <param name="operator">A terminal representing an operator.</param>
/// <param name="numStack">A stack of Number terminals.</param>
///
/// <returns>
/// The number stack with the outcome of the operation prepended.
/// </returns>
let private performOperation operator numStack =
    match operator with
    | Rpar
    | Lpar ->
        ExecError "Execution Error: Parenthesis encountered as an operator."
        |> raise
    | UnaryMinus
    | UnaryPlus ->
        match numStack with
        | [] ->
            ExecError "Execution Error: Unary operation called without any remaining operands."
            |> raise
        | [ Number f ] -> (performUnaryOperation operator f) :: []
        | _ ->
            match numStack.[0] with
            | Number f ->
                (performUnaryOperation operator f)
                :: numStack.[1..]
            | _ ->
                ExecError "Execution Error: Number stack contains non-number tokens."
                |> raise
    | _ ->
        match numStack with
        | [] ->
            ExecError "Execution Error: Binary operation called without any operands."
            |> raise
        | [ Number _ ] ->
            ExecError "Execution Error: Binary operation called with only one operand."
            |> raise
        | [ Number f; Number g ] -> (performBinaryOperation operator g f) :: []
        | _ ->
            match numStack.[0], numStack.[1] with
            | Number f, Number g ->
                (performBinaryOperation operator g f)
                :: numStack.[2..]
            | _ ->
                ExecError "Execution Error: Number stack contains non-number tokens."
                |> raise

/// <summary>
/// Reads a list of terminals, prepending them to an output list, up to a Comma or final Rpar terminal
/// </summary>
///
/// <param name="inList">
/// A list of terminals representing zero or more comma separated assignments followed by a right parenthesis.
/// </param>
/// <param name="outList">A list to contain a single expression taken from the input list.</param>
///
/// <returns>
/// A tuple containing the input list and the output list with the leftmost expression moved from the input list to
/// the output list.
/// </returns>
let rec private extractExpression inList outList =
    match inList with
    | [ Rpar ] -> ([], List.rev outList)
    | Comma :: inTail -> (inTail, List.rev outList)
    | any :: inTail -> extractExpression inTail (any :: outList)
    | [] ->
        ExecError "Execution Error: Function call missing right parenthesis."
        |> raise

/// <summary>
/// Scans through a list of terminals and returns the string representation of the first variable found
/// </summary>
///
/// <param name="expression">A list of terminals representing an input.</param>
///
/// <returns>A String representation of any variable found, if no variable is found, return "!"</returns>
let rec getVariable expression =
    match expression with
    | Word a :: _ -> a
    | [] -> "!"
    | _ :: tail -> getVariable tail

/// <summary>
/// Checks whether a function contains any variables whose values are not defined in the current environment.
/// </summary>
///
/// <param name="terminals">A list of terminals representing an expression in infix notation.</param>
/// <param name="env">The execution environment for any variables in the expression.</param>
///
/// <returns>
/// Returns true or false depending on whether the expression passed to this function is closed or not
/// </returns>
let rec internal closed (env: Map<string, terminal list>) terminals =
    match terminals with
    | [] -> true
    | Word x :: tail ->
        if env.ContainsKey x && closed env env.[x] then
            closed env tail
        else
            false
    | Function a :: tail ->
        match a with
        | "zeroCrossing"
        | "integrate" ->
            let _, remaining = extractParameters tail []
            closed env remaining
        | "differentiate" ->
            let parameters, remaining = extractParameters tail []

            if differentiationClosed parameters env then
                closed env remaining
            else
                false
        | FunctionMatch _ ->
            let parameters, remaining = extractParameters tail []

            if systemFunctionClosed parameters env then
                closed env remaining
            else
                false
        | _ ->
            let remaining, bracketedExpression = extractBrackets tail 0 []
            let newEnv, _ = setArguments bracketedExpression env

            if closed newEnv [ Word a ] then
                closed env remaining
            else
                ExecError "Execution Error: User defined function is not closed by provided assignments"
                |> raise
    | _ :: tail -> closed env tail

/// <summary>Function to check whether a differentiation expression is closed.</summary>
///
/// <param name="parameters">
///     A list of terminal lists representing the parameters to a differentiation expression.
/// </param>
/// <param name="env">The environment in which to check if this expression is closed.</param>
///
/// <returns>A boolean value representing whether the expression is closed.</returns>
and private differentiationClosed (parameters: terminal list list) (env: Map<string, terminal list>) =
    if parameters.Length > 1 then
        if checkUniqueVariables parameters.[0] Set.empty then
            ExecError
                "Execution Error: Partial differentiation is not supported, please only differentiate single variate expressions."
            |> raise
        else
            let variable = getVariable parameters.[0]
            let newEnv = env.Add(variable, parameters.[1])
            let diffedExpr = differentiate parameters.[0]
            closed newEnv diffedExpr
    else
        closed env (differentiate parameters.[0])

/// <summary>Function to check whether a system function expression is closed.</summary>
///
/// <param name="parameters">
///     A list of terminal lists representing the parameters to a system function expression.
/// </param>
/// <param name="env">The environment in which to check if this expression is closed.</param>
///
/// <returns>A boolean value representing whether the expression is closed.</returns>
and private systemFunctionClosed (parameters: terminal list list) (env: Map<string, terminal list>) =
    match parameters with
    | [] -> true
    | head :: tail -> closed env head && systemFunctionClosed tail env

/// <summary>
/// Recursively performs the Dijkstra's Shunting Yard algorithm by reading a terminal list representing an infix
/// expression into an operator stack and a number stack. Performs calculations depending on precedence and
/// associativity rather than producing a reverse Polish notation output.
/// </summary>
///
/// <param name="terminals">A queue of terminals representing an expression in infix notation.</param>
/// <param name="opStack">A stack of operator terminals to compute the result of the infix expression.</param>
/// <param name="numStack">A stack of Number terminals to compute the result of the infix expression.</param>
/// <param name="env">The execution environment for any variables in the expression.</param>
///
/// <returns>A Number terminal containing the outcome of the expression.</returns>
let rec private reduceRecursive terminals opStack numStack (env: Map<string, terminal list>) =
    match terminals with
    | terminalHead :: terminalTail ->
        match terminalHead with
        | Number _ -> reduceRecursive terminalTail opStack (terminalHead :: numStack) env
        | Word x ->
            if env.ContainsKey x then
                reduceRecursive terminalTail opStack (reduce env.[x] env :: numStack) env
            else
                ExecError "Execution Error: Expression with unbound variables passed to reduce."
                |> raise
        | Function a ->
            let remainingTerminals, bracketedExpression = extractBrackets terminalTail 0 []
            let value = handleFunction a bracketedExpression env
            reduceRecursive remainingTerminals opStack (value :: numStack) env
        | Lpar -> reduceRecursive terminalTail (terminalHead :: opStack) numStack env
        | Rpar ->
            match evaluateBrackets opStack numStack performOperation with
            | opStack, numStack -> reduceRecursive terminalTail opStack numStack env
        | UnaryMinus
        | UnaryPlus
        | Divide
        | Times
        | Minus
        | Plus
        | Exponent ->
            match opStack with
            | [] -> reduceRecursive terminalTail (terminalHead :: opStack) numStack env
            | opHead :: opTail ->
                match opHead with
                | Lpar -> reduceRecursive terminalTail (terminalHead :: opStack) numStack env
                | _ ->
                    if (getPrecedence terminalHead < getPrecedence opHead
                        || getPrecedence terminalHead = getPrecedence opHead
                           && getAssociativity terminalHead = "l") then
                        reduceRecursive terminals opTail (performOperation opHead numStack) env
                    else
                        reduceRecursive terminalTail (terminalHead :: opStack) numStack env
        | _ ->
            ExecError "Execution Error: Invalid terminal passed to reduce."
            |> raise
    | [] ->
        match opStack with
        | [] ->
            match numStack with
            | [ _ ] -> numStack.[0]
            | _ ->
                ExecError "Execution Error: Reduction did not result in exactly one terminal in the number stack"
                |> raise
        | Lpar :: _ ->
            ExecError "Execution Error: Unmatched left parenthesis."
            |> raise
        | head :: tail -> reduceRecursive terminals tail (performOperation head numStack) env

/// <summary>Function to handle the reduction of functions.</summary>
///
/// <param name="funcName">The function to be computed.</param>
/// <param name="bracketedExpression">The parameters to the function.</param>
/// <param name="env">The environment in which to reduce the function.</param>
///
/// <returns>A Number terminal representing the reduced value of the function.</returns>
and private handleFunction funcName bracketedExpression env =
    match funcName with
    | "integrate" ->
        let extractedParams, _ =
            extractParameters bracketedExpression []

        if extractedParams.Length <> 3 then
            ExecError "Execution Error: Integration requires three arguments"
            |> raise
        else
            let expression = extractedParams.[0]

            if checkUniqueVariables expression Set.empty then
                ExecError
                    "Execution Error: Integration can only take place with respect to at most one variable, prior variable assignments are not recognised for this purpose"
                |> raise
            else
                let lowerBound = reduce extractedParams.[1] Map.empty
                let upperBound = reduce extractedParams.[2] Map.empty

                if lowerBound >= upperBound then
                    ExecError "Execution Error: Lower bound must be less than upper bound"
                    |> raise
                else
                    let variable = getVariable expression

                    match lowerBound, upperBound with
                    | Number a, Number b -> calculateIntegral expression variable a b ((b - a) / 100.0) 0.0
                    | _ ->
                        ExecError "Execution Error: Reduction did not result in Number."
                        |> raise
    | "differentiate" ->
        let extractedParams, _ =
            extractParameters bracketedExpression []

        if extractedParams.Length = 2 then
            if checkUniqueVariables extractedParams.[0] Set.empty then
                ExecError
                    "Execution Error: Partial differentiation is not supported, please only differentiate single variate expressions."
                |> raise
            else
                let variable = getVariable extractedParams.[0]
                let diffEnv = env.Add(variable, extractedParams.[1])
                reduce (differentiate extractedParams.[0]) diffEnv
        elif extractedParams.Length = 2 then
            reduce (differentiate extractedParams.[0]) env
        else
            ExecError "Execution Error: Differentiation can only occur with 1 or 2 arguments."
            |> raise
    | "zeroCrossing" ->
        let extractedParams, _ =
            extractParameters bracketedExpression []

        if extractedParams.Length <> 2 then
            ExecError "Execution Error: Zero Crossing requires exactly two arguments"
            |> raise
        else
            let expression = extractedParams.[0]
            let seed = extractedParams.[1]
            let A = getVariable expression
            let B = getVariable seed

            if B <> "!" then
                ExecError "Execution Error: Seed Value must be a number"
                |> raise
            else
                let reducedSeed = reduce seed Map.empty

                if checkUniqueVariables expression Set.empty
                   || A = "!" then
                    ExecError
                        "Execution Error: Zero Crossings can only take place with respect to at most one variable, prior variable assignments are not recognised for this purpose"
                    |> raise
                else
                    zeroCrossings expression reducedSeed A 0

    | "sqrt" -> handleRootFunction env bracketedExpression [ Number 2.0 ]
    | "cbrt" -> handleRootFunction env bracketedExpression [ Number 3.0 ]
    | "xrt" ->
        let extractedParams, _ =
            extractParameters bracketedExpression []

        handleRootFunction env extractedParams.[0] extractedParams.[1]
    | "ln" -> handleSingleArgumentFunction env LogETerminal bracketedExpression
    | "logTwo" -> handleSingleArgumentFunction env Log2 bracketedExpression
    | "logTen" -> handleSingleArgumentFunction env Log10 bracketedExpression
    | "floor" -> handleSingleArgumentFunction env FloorToTerminal bracketedExpression
    | "ceil" -> handleSingleArgumentFunction env CeilToTerminal bracketedExpression
    | "round" -> handleSingleArgumentFunction env RoundNum bracketedExpression
    | "abs" -> handleSingleArgumentFunction env AbsVal bracketedExpression
    | "logX" -> handleTwoArgumentFunction env LogX bracketedExpression
    | "gcd" -> handleTwoArgumentFunction env getGCDWrapper bracketedExpression
    | "mod" -> handleTwoArgumentFunction env moduloCalc bracketedExpression
    | "rand" -> handleTwoArgumentFunction env pseudoRandom bracketedExpression

    | "pi" -> Number 3.1415
    | "euler" -> Number 2.7183
    | _ ->
        if env.ContainsKey funcName then
            let newEnv, _ = setArguments bracketedExpression env

            let combinedEnv =
                Map.fold (fun acc key value -> Map.add key value acc) env newEnv

            (reduce [ Word funcName ] combinedEnv)
        else
            ExecError "Execution Error: Assignments given in function call do not close expression."
            |> raise


/// <summary>
/// Function to control the evaluation of a zero crossing of a single variate function.
/// </summary>
///
/// <param name="expression">The expression for which to find the zero crossing.</param>
/// <param name="seed">The seed value to be used to finding the zero crossing.</param>
/// <param name="variable">The variable in which this function is in.</param>
/// <param name="count">The number of executions of zero crossings so far, to prevent infinite looping.</param>
///
/// <returns>A Number terminal containing the zero crossing.</returns>
and private zeroCrossings expression seed variable count=
    match count with
    | 200 -> ExecError
                 ("Execution Error: Could Not Find a root for function : \"" + terminalListToString "" expression + "\" Please try another estimate or plot the function to see if there are any crossings")
             |> raise
    | _ ->
        let resultA =
            reduce expression (Map.empty.Add(variable, [ seed ]))

        let resultB =
            reduce (differentiate expression) (Map.empty.Add(variable, [ seed ]))

        let resultC =
            reduce
                [ seed
                  Minus
                  resultA
                  Divide
                  resultB ]
                Map.empty

        match resultC, seed with
        | Number A, Number B ->
            if abs (A - B) < 0.01 then
                A |> Number
            else
                zeroCrossings expression resultC variable (count+1)
        | _ ->
            ExecError "Execution Error: Zero Crossings Could Not Find A Proper root"
            |> raise

/// <summary>
/// Function to control the calculation of an integral of a single variate function using the trapezoid rule.
/// </summary>
///
/// <param name="expression">The expression for which to find the integral.</param>
/// <param name="variable">The variable in which this function is in.</param>
/// <param name="lowerBound">The lowerBound of the definite integral.</param>
/// <param name="current">The highest value for which a trapezoid area has not been calculated.</param>
/// <param name="step">The trapezoid width.</param>
/// <param name="sum">The current area calculated.</param>
///
/// <returns>An estimate of the area under the curve between the lowerBound and the initial upper bound.</returns>
and private calculateIntegral expression variable lowerBound current step sum  =
    if current <= lowerBound then
        Number sum
    else
        let resultA =
            reduce expression (Map.empty.Add(variable, [ Number(float current) ]))

        let resultB =
            reduce expression (Map.empty.Add(variable, [ Number(float (current - step)) ]))

        match resultA, resultB with
        | Number a, Number b ->
            calculateIntegral expression variable lowerBound (current - step) step (sum + step * (a - ((a - b) / 2.0)))
        | _ ->
            ExecError "Execution Error: Reduction did not result in Numbers"
            |> raise

/// <summary>
/// Function to control the calculation of a root.
/// </summary>
///
/// <param name="env">The environment to be used for any variables.</param>
/// <param name="operand">The operand in the root expression.</param>
/// <param name="exponent">The denominator of the exponent in the root expression.</param>
///
/// <returns>A number terminal representing the value of the root function..</returns>
and private handleRootFunction env operand exponent =
    let reducedOperand = reduce operand env
    let reducedExponent = reduce exponent env

    match reducedOperand, reducedExponent with
    | Number a, Number b ->
        if a < 0.0 && (b < 0.0 || (b % 2.0) <> 1.0) then
            InvalidArgumentError
                "Execution Error: First argument to root function must be positive to take an even numbered root."
            |> raise
        else
            reduce
                (RootToTerminals [ reducedOperand ] [
                    reducedExponent
                 ])
                env
    | _ -> ExecError "error" |> raise

/// <summary>
/// Function to handle predefined functions with one arguments, contains logic to ensure exactly one argument is
/// passed.
/// </summary>
///
/// <param name="env">A list of terminals representing an expression in infix notation.</param>
/// <param name="func">The execution environment for any variables in the expression.</param>
/// <param name="expression">The expression to be evaluated.</param>
///
/// <returns>A Number terminal containing the outcome of the expression.</returns>
and private handleSingleArgumentFunction env func expression =
    let extractedParams, _ = extractParameters expression []

    if extractedParams.Length <> 1
       || extractedParams.[0].Length = 0 then
        ExecError "Execution Error: Function requires exactly one argument"
        |> raise
    else
        match reduce expression env with
        | Number a -> func a
        | _ ->
            ExecError "Execution Error: Expected number to be passed as argument but received otherwise"
            |> raise

/// <summary>
/// Function to handle predefined functions with two arguments, contains logic to ensure exactly two arguments are
/// passed.
/// </summary>
///
/// <param name="env">A list of terminals representing an expression in infix notation.</param>
/// <param name="func">The execution environment for any variables in the expression.</param>
/// <param name="expression">The expression to be evaluated.</param>
///
/// <returns>A Number terminal containing the outcome of the expression.</returns>
and private handleTwoArgumentFunction env func expression =
    let extractedParams, _ = extractParameters expression []

    if extractedParams.Length <> 2 then
        ExecError "Execution Error: Function requires two arguments."
        |> raise
    else
        let baseValue = reduce extractedParams.[0] env
        let operand = reduce extractedParams.[1] env

        match baseValue, operand with
        | Number a, Number b -> reduce [ func a b ] env
        | _ -> ExecError "error" |> raise

/// <summary>
/// Wrapper for reduceRecursive to call it with empty operator and number stacks.
/// </summary>
///
/// <param name="terminals">A list of terminals representing an expression in infix notation.</param>
/// <param name="env">The execution environment for any variables in the expression.</param>
///
/// <returns>A Number terminal containing the outcome of the expression.</returns>
and private reduce terminals (env: Map<string, terminal list>) = reduceRecursive terminals [] [] env

/// <summary>
/// Recursively execute differentiate on any input which has a nested differentiate and return a terminal list of the
/// new expanded input.
/// </summary>
///
/// <param name="terminalsIn">A list of terminals representing an input.</param>
/// <param name="terminalsOut">A list of terminals representing a converted expression whenever there is a nested
/// differentiation.
/// </param>
/// <param name="env">A map representing currently assigned user defined variables.</param>
///
/// <returns>
/// A list of terminals representing a converted expression after executing a nested differentiation.
/// </returns>
let rec private expandDifferentiates terminalsIn terminalsOut env =
    match terminalsIn with
    | head :: tail when head = Function "differentiate" ->
        let parameters, remaining = extractParameters tail []

        if parameters.Length = 1 then
            expandDifferentiates
                remaining
                (terminalsOut
                 @ [ Lpar ]
                   @ (differentiate parameters.[0]) @ [ Rpar ])
                env
        else
            let reduction =
                reduce
                    (Function "differentiate"
                     :: (extractBrackets tail 0 [] |> fst))
                    env

            expandDifferentiates remaining (terminalsOut @ [ reduction ]) env
    | head :: tail -> expandDifferentiates tail (terminalsOut @ [ head ]) env
    | [] -> terminalsOut

/// <summary>
/// Function to check whether an assignment expression forms a circular assignment.
/// </summary>
///
/// <param name="environment">The environment in which to check for circular assignment</param>
/// <param name="variable">The variable that is being assigned.</param>
/// <param name="assignment">The assignment being made to this variable.</param>
///
/// <returns>A tuple containing the result of the expression and an updated execution environment.</returns>
let rec private checkCircularAssignment (environment: Map<string, terminal list>) variable assignment =
    match assignment with
    | Word a :: tail ->
        if a = variable then
            false
        elif environment.ContainsKey a then
            let innerAssignment = environment.[a]
            checkCircularAssignment environment variable innerAssignment
        else
            checkCircularAssignment environment variable tail
    | _ :: tail -> checkCircularAssignment environment variable tail
    | [] -> true

/// <summary>
/// Computes a result, as a terminal list, and an updated execution environment given a terminal list representing
/// a valid statement and an execution environment.
/// </summary>
///
/// <param name="terminals">A list of terminals representing a valid MyMathsPal statement.</param>
/// <param name="env">The current MyMathsPal execution environment.</param>
///
/// <returns>A tuple containing the result of the expression and an updated execution environment.</returns>
let rec internal exec (env: Map<string, terminal list>) terminals =
    match terminals with
    | Word x :: Assign :: tail ->
        if checkCircularAssignment env x tail then
            match tail with
            | Word _ :: Assign :: _ ->
                ExecError "Execution Error: Malformed expression; an assignment may not be assigned to an assignment"
                |> raise
            | [] ->
                ExecError "Execution Error: Malformed expression; an assignment may not be empty"
                |> raise
            | _ ->
                let result, _ = exec env tail
                terminals, (env.Add(x, result) |> Map.toSeq |> dict)
        else
            ExecError "Execution Error: Circular assignment is not permitted."
            |> raise
    | [] -> [], env |> Map.toSeq |> dict
    | _ ->
        if closed env terminals then
            let result = [ reduce terminals env ]
            result, (env |> Map.toSeq |> dict)
        else
            let result = expandDifferentiates terminals [] env
            result, (env |> Map.toSeq |> dict)
