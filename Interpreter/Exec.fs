/// <summary>
/// Module containing functions for execution of commands passed to the MyMathsPal Interpreter.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module Interpreter.Exec

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
let performBinaryOperation operator op1 op2 =
    match operator with
    | Plus -> Number (op1 + op2)
    | Minus -> Number (op1 - op2)
    | Times -> Number (op1 * op2)
    | Divide ->
        match op2 with
        | 0.0 -> CalculateError "Calculate Error: Attempting to divide by zero, this operation is undefined." |> raise
        | _ -> Number (op1 / op2)
    | Exponent -> Number (op1 ** op2)
    | _ -> CalculateError "Calculate Error: Invalid operator passed." |> raise

/// <summary>
/// Performs a unary arithmetic operation given terminals representing a unary operator and an operand.
/// </summary>
/// 
/// <param name="operator">A terminal representing an operator.</param>
/// <param name="operand">A Number terminal containing the operand.</param>
///
/// <returns>A Number containing the result of the operation.</returns>
let performUnaryOperation operator operand =
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
let performOperation operator numStack =
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
let rec reduceRecursive terminals opStack numStack (env: Map<string, terminal list>) =
    match terminals with
    | terminalHead :: terminalTail ->
        match terminalHead with
        | Number _ -> reduceRecursive terminalTail opStack (terminalHead :: numStack) env
        | Word x ->
            if env.ContainsKey x
            then reduceRecursive terminalTail opStack (reduce env.[x] env :: numStack) env
            else ExecError "Execution Error: Expression with unbound variables passed to reduce." |> raise
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

/// <summary>
/// Wrapper for reduceRecursive to call it with empty operator and number stacks.
/// </summary>
///
/// <param name="terminals">A list of terminals representing an expression in infix notation.</param>
/// <param name="env">The execution environment for any variables in the expression.</param>
///
/// <returns>A Number terminal containing the outcome of the expression.</returns>
and reduce terminals (env: Map<string, terminal list>) =
    reduceRecursive terminals [] [] env

/// <summary>
/// Checks whether a function contains any variables whose values are not defined in the current environment.
/// </summary>
///
/// <param name="terminals">A list of terminals representing an expression in infix notation.</param>
/// <param name="env">The execution environment for any variables in the expression.</param>
///
/// <returns></returns>
let rec closed terminals (env: Map<string, terminal list>) =
    match terminals with
    | [] -> true
    | Word x :: tail ->
        if env.ContainsKey x && closed env.[x] env
        then closed tail env
        else false
    | _ :: tail -> closed tail env

/// <summary>
/// Reads a list of terminals, prepending them to an output list, up to a Comma or Rpar terminal.
/// </summary>
///
/// <param name="inList">
/// A list of terminals representing zero or more comma separated assignments followed by a right parenthesis.
/// </param>
/// <param name="outList">A list to contain a single assignment expression taken from the input list.</param>
///
/// <returns>
/// A tuple containing the input list and the output list with the leftmost assignation moved from the input list to
/// the output list.
/// </returns>
let rec extractAssignment inList outList =
    match inList with
    | Rpar :: _ -> (inList, List.rev outList)
    | Comma :: inTail -> (inTail, List.rev outList)
    | any :: inTail -> extractAssignment inTail (any :: outList)
    | [] -> ExecError "Execution Error: Function call missing right parenthesis." |> raise

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
let rec extractExpression inList outList =
    match inList with
    | [ Rpar ] -> ([], List.rev outList)
    | Comma :: inTail -> (inTail, List.rev outList)
    | any :: inTail -> extractExpression inTail (any :: outList)
    | [] -> ExecError "Execution Error: Function call missing right parenthesis." |> raise
    
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
let rec setArguments terminals (env: Map<string, terminal list>) =
    match terminals with
    | Rpar :: _ -> env
    | Word x :: Assign :: tail ->
        match extractAssignment tail [] with
        | a, b -> setArguments a (env.Add(x, [reduce b env] ))
    | _ -> ExecError "Execution Error: Function call contains non-assignment expression." |> raise
    

/// <summary>
/// Computes a result, as a terminal list, and an updated execution environment given a terminal list representing
/// a valid statement and an execution environment.
/// </summary>
///
/// <param name="terminals">A list of terminals representing a valid MyMathsPal statement.</param>
/// <param name="env">The current MyMathsPal execution environment.</param>
///
/// <returns>A tuple containing the result of the expression and an updated execution environment.</returns>
let rec exec terminals (env: Map<string, terminal list>) =
    match terminals with
    | Word x :: Assign :: tail ->
        match tail with
        | Word _ :: Assign :: _ -> ExecError "Execution Error: Malformed expression; an assignment may not be assigned to an assignment" |> raise
        | _ ->
            let result, _ = exec tail env
            terminals, (env.Add(x, result) |> Map.toSeq |> dict) 
    | Word x :: Lpar :: tail ->
        //https://stackoverflow.com/questions/3974758/in-f-how-do-you-merge-2-collections-map-instances
        let newEnv = setArguments tail Map.empty
        let combinedEnv = Map.fold (fun acc key value -> Map.add key value acc) env newEnv
        
        if closed [Word x] combinedEnv
        then [reduce [Word x] combinedEnv], (env |> Map.toSeq |> dict)
        else ExecError "Execution Error: Assignments given in function call do not close expression." |> raise
    | Function a :: tail ->
        match a with
        | "differentiate" ->
            match tail.[0], tail.[tail.Length-1] with
            | Lpar, Rpar ->
                let assignment, expression = extractExpression tail.[1..] []
                if assignment <> []
                then
                    if (List.filter (fun x -> x = Comma) assignment).Length = 0
                    then
                        let newEnv = setArguments assignment env
                        if closed expression newEnv
                        then
                            [reduce (differentiate expression) newEnv], (env |> Map.toSeq |> dict)
                        else ExecError "Execution Error: Assignment does not match variable in expression" |> raise
                    else ExecError "" |> raise
                else
                    differentiate expression, (env |> Map.toSeq |> dict)
            | _ -> ExecError "Execution Error: Malformed expression; missing parenthesis enclosing expression" |> raise
        | "sqrt" ->
            //Set assignment to any assignments inside of expression
            let assignment, expression = extractExpression tail.[1..] []
            if assignment <> []
            then
                ExecError "Execution Error: Unexpected Assignment" |> raise
            else
                [reduce (RootToTerminals expression 2.0) env], (env |> Map.toSeq |> dict)
        | "cbrt" ->
            let assignment, expression = extractExpression tail.[1..] []
            if assignment <> []
            then
                ExecError "Execution Error" |> raise
            else
                [reduce (RootToTerminals expression 3.0) env], (env |> Map.toSeq |> dict)
        | "ln" ->
            let assignment, expression = extractExpression tail.[1..] []
            if assignment <> []
            then
                ExecError "Execution Error: Unexpected Assignment Between Parenthesis For \"ln\"" |> raise
            else
                match reduce expression env with
                | Number a ->
                    [LogWrapperToTerminal LogE a], (env |> Map.toSeq |> dict)
                | _ -> ExecError "Execution Error: Invalid " |> raise
        | "logTwo" ->
            let assignment, expression = extractExpression tail.[1..] []
            if assignment <> []
            then
                ExecError "Execution Error: Unexpected Assignment Between Parenthesis For \"log2\"" |> raise
            else
                match reduce expression env with
                | Number a ->
                    [LogWrapperToTerminal Log2 a], (env |> Map.toSeq |> dict)
                | _ -> ExecError "Execution Error: Invalid " |> raise
        | "logTen" ->
            let assignment, expression = extractExpression tail.[1..] []
            if assignment <> []
            then
                ExecError "Execution Error: Unexpected Assignment Between Parenthesis For \"log10\"" |> raise
            else
                match reduce expression env with
                | Number a ->
                    [LogWrapperToTerminal Log10 a], (env |> Map.toSeq |> dict)
                | _ -> ExecError "Execution Error: Invalid " |> raise
        | "floor" ->
            let assignment, expression = extractExpression tail.[1..] []
            if assignment <> []
            then
                ExecError "Execution Error: Unexpected Assignment Between Parenthesis For \"log10\"" |> raise
            else
                match reduce expression env with
                | Number a ->
                    [NumToTerminal (FloorToNumber a)], (env |> Map.toSeq |> dict)
                | _ -> ExecError "Execution Error: Invalid " |> raise
        | "ceil" ->
            let assignment, expression = extractExpression tail.[1..] []
            if assignment <> []
            then
                ExecError "Execution Error: Unexpected Assignment Between Parenthesis For \"log10\"" |> raise
            else
                match reduce expression env with
                | Number a ->
                    [NumToTerminal (CeilToNumber a)], (env |> Map.toSeq |> dict)
                | _ -> ExecError "Execution Error: Invalid " |> raise
        | "round" ->
            let assignment, expression = extractExpression tail.[1..] []
            if assignment <> []
            then
                ExecError "Execution Error: Unexpected Assignment Between Parenthesis For \"round\"" |> raise
            else
                match reduce expression env with
                | Number a ->
                    [NumToTerminal (round a)], (env |> Map.toSeq |> dict)
                | _ -> ExecError "Execution Error: Invalid " |> raise
        | "abs" ->
            let assignment, expression = extractExpression tail.[1..] []
            if assignment <> []
            then
                ExecError "Execution Error: Unexpected Assignment Between Parenthesis For \"round\"" |> raise
            else
                match reduce expression env with
                | Number a ->
                    [NumToTerminal (abs a)], (env |> Map.toSeq |> dict)
                | _ -> ExecError "Execution Error: Invalid " |> raise
        | _ -> ExecError "Execution Error: Malformed expression; undefined function called" |> raise
    | _ ->
        if closed terminals env
        then [reduce terminals env], (env |> Map.toSeq |> dict)
        else terminals, (env |> Map.toSeq |> dict)
        
        
//Create extractArguments to read comma seperated list ending with )