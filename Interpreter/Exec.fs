/// <summary>
/// Module containing functions for execution of commands passed to the MyMathsPal Interpreter.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module internal Interpreter.Exec

open Interpreter
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
    | Plus -> Number (op1 + op2)
    | Minus -> Number (op1 - op2)
    | Times -> Number (op1 * op2)
    | Divide ->
        match op2 with
        | 0.0 -> CalculateError "Calculate Error: Attempting to divide by zero, this operation is undefined." |> raise
        | _ -> Number (op1 / op2)
    | Exponent ->
        if op1 <= 0.0 && 0.0 < op2 && op2 < 1.0 then
            Number (-((-op1)**op2))
        else
            Number (op1 ** op2)
    | _ -> CalculateError "Calculate Error: Invalid operator passed." |> raise

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
    | _ -> UnaryError "Unary Error: Invalid operator passed." |> raise

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
    | Lpar -> ExecError "Execution Error: Parenthesis encountered as an operator." |> raise
    | UnaryMinus
    | UnaryPlus ->
        match numStack with
        | [] -> ExecError "Execution Error: Unary operation called without any remaining operands." |> raise
        | [ Number f; ] -> (performUnaryOperation operator f) :: []
        | _ ->
            match numStack.[0] with
            | Number f -> (performUnaryOperation operator f) :: numStack.[1 .. ]
            | _ -> ExecError "Execution Error: Number stack contains non-number tokens." |> raise
    | _ ->
        match numStack with
        | [] -> ExecError "Execution Error: Binary operation called without any operands." |> raise
        | [ Number _; ] -> ExecError "Execution Error: Binary operation called with only one operand." |> raise
        | [ Number f; Number g; ] -> (performBinaryOperation operator g f) :: []
        | _ ->
            match numStack.[0], numStack.[1] with
            | Number f, Number g -> (performBinaryOperation operator g f) :: numStack.[2 .. ]
            | _ -> ExecError "Execution Error: Number stack contains non-number tokens." |> raise

let rec private extractBrackets terminals lparCount out =
    match terminals with
    | Lpar :: tail -> extractBrackets tail (lparCount+1) (Lpar::out)
    | Rpar :: tail ->
        match lparCount with
        | 1 -> tail, List.rev (Rpar :: out)
        | _ -> extractBrackets tail (lparCount-1) (Rpar::out)
    | any :: tail -> extractBrackets tail lparCount (any::out)
    | [] -> ExecError "Execution Error: Unmatched parenthesis." |> raise

/// <summary>
/// Reads a list of terminals, prepending them to an output list, up to a Comma or Rpar terminal.
/// </summary>
///
/// <param name="inList">
/// A list of terminals representing zero or more comma separated assignments followed by a right parenthesis.
/// </param>
/// <param name="outList">A list to contain a single assignment expression taken from the input list.</param>
/// <param name="nestCount">Maintain a count of left and right parenthesis</param>
/// <returns>
/// A tuple containing the input list and the output list with the leftmost assignation moved from the input list to
/// the output list.
/// </returns>
let rec private extractAssignment inList outList nestCount =
    match inList with
    | Rpar :: _ when nestCount = 0 -> inList, List.rev outList
    | [Rpar] -> inList, List.rev outList
    | Comma :: _ when nestCount = 0 -> (inList, List.rev outList)
    | Lpar :: inTail -> extractAssignment inTail (Lpar :: outList) (nestCount+1)
    | Rpar :: inTail -> extractAssignment inTail (Rpar :: outList) (nestCount-1)
    | any :: inTail -> extractAssignment inTail (any :: outList) nestCount
    | [] -> inList, List.rev outList

/// <summary>
/// Creates an environment from a list of terminals representing Comma separate assignments.
/// </summary>
///
/// <param name="terminals">
/// A list of terminals representing zero or more comma separated assignments followed by a right parenthesis.
/// </param>
/// <param name="env">The execution environment in which the variable assignments are to be stored.</param>
///
/// <returns></returns>
and private setArguments terminals (env: Map<string, terminal list>) =
    match terminals with
    | Rpar :: tail -> env, tail
    | Word x :: Assign :: tail ->
        match extractAssignment tail [] 0 with
        | remaining, expression ->
            setArguments remaining (env.Add(x, expression))
    | Comma :: tail
    | Lpar :: tail -> setArguments tail env
    | _ -> env, terminals

and private extractParameters terminals (paramList : terminal list list) env =
    match terminals with
    | Rpar :: tail -> List.rev paramList, tail
    | Comma :: tail
    | Lpar :: tail ->
        let remaining, parameter = extractAssignment tail [] 0
        extractParameters remaining (parameter :: paramList) env
    | [] -> ExecError "Execution Error: Unmatched left parenthesis" |> raise
    | _ -> ExecError "Execution Error: Unmatched right parenthesis" |> raise

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
    | [] -> ExecError "Execution Error: Function call missing right parenthesis." |> raise

/// <summary>
/// Checks whether a function contains any variables whose values are not defined in the current environment.
/// </summary>
///
/// <param name="terminals">A list of terminals representing an expression in infix notation.</param>
/// <param name="env">The execution environment for any variables in the expression.</param>
///
/// <returns></returns>
let rec internal closed (env: Map<string, terminal list>) terminals  =
    match terminals with
    | [] -> true
    | Word x :: tail ->
        if env.ContainsKey x && closed env env.[x]
        then closed env tail
        else false
    | Function a :: tail ->
        match a with
        | "integrate" ->
            let _, remaining = extractParameters tail [] env
            closed env remaining
        | "differentiate" ->
            let parameters, remaining = extractParameters tail [] env
            if differentiationClosed parameters env then
                closed env remaining
            else false
        | FunctionMatch _ ->
            let parameters, remaining = extractParameters tail [] env
            if systemFunctionClosed parameters env then
                closed env remaining
            else false
        | _ ->
            let remaining, bracketedExpression = extractBrackets tail 0 []
            let newEnv, _ = setArguments bracketedExpression env
            if closed newEnv [Word a] then
                closed env remaining
            else ExecError "Execution Error: User defined function is not closed by provided assignments" |> raise
    | _ :: tail -> closed env tail

and private differentiationClosed (parameters: terminal list list) (env: Map<string, terminal list>) =
    if parameters.Length > 1 then
        let newEnv, _ = setArguments parameters.[1] env
        let diffedExpr = differentiate parameters.[0]
        closed (toMap newEnv) diffedExpr
    else
        closed env (differentiate parameters.[0])

and private systemFunctionClosed (parameters: terminal list list) (env: Map<string, terminal list>) =
    match parameters with
    | [] -> true
    | head :: tail ->
        closed env head && systemFunctionClosed tail env

let rec getVariable expression =
    match expression with
    | Word a :: tail -> a
    | [] -> "!"
    | _ :: tail -> getVariable tail

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
            if env.ContainsKey x
            then reduceRecursive terminalTail opStack (reduce env.[x] env :: numStack) env
            else ExecError "Execution Error: Expression with unbound variables passed to reduce." |> raise
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
                           && getAssociativity terminalHead = "l")
                    then reduceRecursive terminals opTail (performOperation opHead numStack) env
                    else reduceRecursive terminalTail (terminalHead :: opStack) numStack env
        | _ -> ExecError "Execution Error: Invalid terminal passed to reduce." |> raise
    | [] ->
        match opStack with
        | [] ->
            match numStack with
            | [ _ ] -> numStack.[0]
            | _ -> ExecError "Execution Error: Reduction did not result in exactly one terminal in the number
                              stack" |> raise
        | Lpar :: _ -> ExecError "Execution Error: Unmatched left parenthesis." |> raise
        | head :: tail -> reduceRecursive terminals tail (performOperation head numStack) env

and private handleFunction funcName bracketedExpression env : terminal=
    match funcName with
    | "integrate" ->
        let extractedParams, _ = extractParameters bracketedExpression [] env

        if extractedParams.Length <> 3 then ExecError "Execution Error: Integration requires three arguments" |> raise
        else
            let expression = extractedParams.[0]
            if checkUniqueVariables expression Set.empty then
                ExecError "Execution Error: Integration can only take place with respect to at most one variable, prior variable assignments are not recognised for this purpose" |> raise
            else
                let lowerBound = reduce extractedParams.[1] Map.empty
                let upperBound = reduce extractedParams.[2] Map.empty

                if lowerBound > upperBound then
                    ExecError "Execution Error: Lower bound must be less than upper bound" |> raise
                else
                    let variable = getVariable expression
                    match lowerBound, upperBound with
                    | Number a, Number b ->
                        calculateIntegral expression variable (int a) (int b) 0.0
                    | _ -> ExecError "Execution Error: Reduction did not result in Number." |> raise
    | "differentiate" ->
        let extractedParams, _ = extractParameters bracketedExpression [] env
        if extractedParams.Length > 1 then
            let diffEnv, _ = setArguments extractedParams.[1] env
            reduce (differentiate extractedParams.[0]) (toMap diffEnv)
        else
            reduce (differentiate extractedParams.[0]) env
    | "sqrt" -> handleRootFunction env bracketedExpression [Number 2.0]
    | "cbrt" -> handleRootFunction env bracketedExpression [Number 3.0]
    | "xrt" ->
        let extractedParams, _ = extractParameters bracketedExpression [] env
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
    | "pi" -> Number 3.14
    | "euler" -> Number 2.71
    | _ ->
        if env.ContainsKey funcName then
            let newEnv, _ = setArguments bracketedExpression env
            let combinedEnv = Map.fold (fun acc key value -> Map.add key value acc) env newEnv
            (reduce [Word funcName] combinedEnv)
        else ExecError "Execution Error: Assignments given in function call do not close expression." |> raise

and calculateIntegral expression variable lowerBound (current : int) sum =
    if current = lowerBound then Number sum
    else
        let resultA = reduce expression (Map.empty.Add (variable, [Number (float current)]))
        let resultB = reduce expression (Map.empty.Add (variable, [Number (float (current-1))]))
        match resultA, resultB with
        | Number a, Number b -> calculateIntegral expression variable lowerBound (current-1) (sum + (a - ((a - b)/2.0)))
        | _ -> ExecError "Execution Error: Reduction did not result in Numbers" |> raise

and private handleRootFunction (env : Map<string, terminal list>) operand exponent =
    let reducedOperand = reduce operand env
    let reducedExponent = reduce exponent env
    match reducedOperand, reducedExponent with
    | Number a, Number b ->
        if a < 0.0 && (b < 0.0 || (b % 2.0) <> 1.0) then
            InvalidArgumentError "First argument to root function must be positive to take an even numbered root." |> raise
        else
            reduce (RootToTerminals [reducedOperand] [reducedExponent]) env
    | _ ->
        ExecError "error" |> raise

and private handleSingleArgumentFunction env func expression =
    let extractedParams, _ = extractParameters expression [] env
    if extractedParams.Length <> 1 || extractedParams.[0].Length = 0 then ExecError "Execution Error: Function requires exactly one argument" |> raise
    else
        match reduce expression env with
        | Number a -> func a
        | _ -> ExecError "Execution Error: Expected number to be passed as argument but received otherwise" |> raise

and private handleTwoArgumentFunction env func expression =
    let extractedParams, _ = extractParameters expression [] env

    if extractedParams.Length <> 2 then ExecError "Execution Error: Function requires two arguments." |> raise
    else
        let baseValue = reduce extractedParams.[0] env
        let operand = reduce extractedParams.[1] env
        match baseValue, operand with
        | Number a, Number b ->
            reduce [func a b] env
        | _ ->
            ExecError "error" |> raise

/// <summary>
/// Wrapper for reduceRecursive to call it with empty operator and number stacks.
/// </summary>
///
/// <param name="terminals">A list of terminals representing an expression in infix notation.</param>
/// <param name="env">The execution environment for any variables in the expression.</param>
///
/// <returns>A Number terminal containing the outcome of the expression.</returns>
and private reduce terminals (env: Map<string, terminal list>) =
    reduceRecursive terminals [] [] env

let rec private expandDifferentiates terminalsIn terminalsOut env =
    match terminalsIn with
    | head :: tail when head = Function "differentiate" ->
        let parameters, remaining = extractParameters tail [] env
        if parameters.Length = 1 then
            expandDifferentiates remaining (terminalsOut @ [Lpar] @ (differentiate parameters.[0]) @ [Rpar]) env
        else
            let reduction = reduce (Function "differentiate" :: (extractBrackets tail 0 [] |> fst)) env
            expandDifferentiates remaining (terminalsOut @ [reduction]) env
    | head :: tail -> expandDifferentiates tail (terminalsOut @ [head]) env
    | [] -> terminalsOut

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
        match tail with
        | Word _ :: Assign :: _ -> ExecError "Execution Error: Malformed expression; an assignment may not be assigned to an assignment" |> raise
        | [] -> ExecError "Execution Error: Malformed expression; an assignment may not be empty" |> raise
        | _ ->
            let result, _ = exec env tail
            terminals, (env.Add(x, result) |> Map.toSeq |> dict)
    | [] -> [], env |> Map.toSeq |> dict
    | _ ->
        if closed env terminals then
            let result = [reduce terminals env]
            result, (env |> Map.toSeq |> dict)
        else
            let result = expandDifferentiates terminals [] env
            result, (env |> Map.toSeq |> dict)
