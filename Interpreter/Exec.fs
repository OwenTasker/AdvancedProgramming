/// <summary>
/// Module containing functions for execution of commands passed to the MyMathsPal Interpreter.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module Interpreter.Exec

open System
open System.Collections.Generic
open System.Text.RegularExpressions
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

let rec extractBrackets terminals lparCount out =
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
///
/// <returns>
/// A tuple containing the input list and the output list with the leftmost assignation moved from the input list to
/// the output list.
/// </returns>
let rec extractAssignment inList outList =
    match inList with
    | Rpar :: _ -> inList, List.rev outList
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
/// Checks whether a function contains any variables whose values are not defined in the current environment.
/// </summary>
///
/// <param name="terminals">A list of terminals representing an expression in infix notation.</param>
/// <param name="env">The execution environment for any variables in the expression.</param>
///
/// <returns></returns>
let rec closed (env: Map<string, terminal list>) terminals  =
    match terminals with
    | [] -> true, env
    | Word x :: tail ->
        if env.ContainsKey x && closed env env.[x] |> fst
        then closed env tail 
        else false, env
    | Function a :: tail ->
        match a with
        | Lexer.FunctionMatch _ ->
            let remaining, _ = extractBrackets tail 0 []
            closed env remaining
        | _ ->
            let newEnv, remainingTerminals = setArguments tail env
            closed newEnv remainingTerminals
    | _ :: tail -> closed env tail
    
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
and setArguments terminals (env: Map<string, terminal list>) =
    match terminals with
    | Rpar :: tail -> env, tail
    | Word x :: Assign :: tail ->
        match extractAssignment tail [] with
        | a, b -> setArguments a (env.Add(x, [reduce b env] ))
    | Lpar :: tail -> setArguments tail env
    | _ -> ExecError "Execution Error: Function call contains non-assignment expression." |> raise

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
and reduceRecursive terminals opStack numStack (env: Map<string, terminal list>) =
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
            
            if env.ContainsKey a
            then
                let terminalList, _ = exec env (Function a :: bracketedExpression)
                reduceRecursive remainingTerminals opStack (terminalList @ numStack) env
            else
                match a with
                | Lexer.FunctionMatch _ ->
                    let terminalList, _ = exec env (Function a :: bracketedExpression)
                    reduceRecursive remainingTerminals opStack (terminalList @ numStack) env
                | _ -> ExecError "Execution Error: Undefined function" |> raise
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
/// Computes a result, as a terminal list, and an updated execution environment given a terminal list representing
/// a valid statement and an execution environment.
/// </summary>
///
/// <param name="terminals">A list of terminals representing a valid MyMathsPal statement.</param>
/// <param name="env">The current MyMathsPal execution environment.</param>
///
/// <returns>A tuple containing the result of the expression and an updated execution environment.</returns>
and exec (env: Map<string, terminal list>) terminals  =
    let expandedTerminals = evaluateDifferentiates terminals env []
    match expandedTerminals with
    | Word x :: Assign :: tail ->
        match tail with
        | Word _ :: Assign :: _ -> ExecError "Execution Error: Malformed expression; an assignment may not be assigned to an assignment" |> raise
        | _ ->
            let result, _ = exec env tail
            expandedTerminals, (env.Add(x, result) |> Map.toSeq |> dict) 
        //https://stackoverflow.com/questions/3974758/in-f-how-do-you-merge-2-collections-map-instances
    | Function a :: tail ->
        match a with
        | "differentiate" ->
            ExecError "Execution Error: Differential not expanded by exec." |> raise
        | "sqrt" ->
            //Set assignment to any assignments inside of expression
            let remaining, bracketedExpression = extractBrackets tail 0 []
            [reduce ((reduce (RootToTerminals bracketedExpression 2.0) env) :: remaining) env], (env |> Map.toSeq |> dict)
        | "cbrt" ->
            let remaining, bracketedExpression = extractBrackets tail 0 []
            [reduce ((reduce (RootToTerminals bracketedExpression 3.0) env) :: remaining) env], (env |> Map.toSeq |> dict)
        | "ln" ->
            let remaining, bracketedExpression = extractBrackets tail 0 []
            match reduce bracketedExpression env with
            | Number a ->
                [reduce ((LogWrapperToTerminal LogE a) :: remaining) env], (env |> Map.toSeq |> dict)
            | _ -> ExecError "Execution Error: Invalid " |> raise
        | "logTwo" ->
            let remaining, bracketedExpression = extractBrackets tail 0 []
            match reduce bracketedExpression env with
            | Number a ->
                [reduce ((LogWrapperToTerminal Log2 a) :: remaining) env], (env |> Map.toSeq |> dict)
            | _ -> ExecError "Execution Error: Invalid " |> raise
        | "logTen" ->
            let remaining, bracketedExpression = extractBrackets tail 0 []
            match reduce bracketedExpression env with
            | Number a ->
                [reduce ((LogWrapperToTerminal Log10 a) :: remaining) env], (env |> Map.toSeq |> dict)
            | _ -> ExecError "Execution Error: Invalid " |> raise
        | "floor" ->
            let remaining, bracketedExpression = extractBrackets tail 0 []
            match reduce bracketedExpression env with
            | Number a ->
                [reduce (FloorToNumber a :: remaining) env], (env |> Map.toSeq |> dict)
            | _ -> ExecError "Execution Error: Invalid " |> raise
        | "ceil" ->
            let remaining, bracketedExpression = extractBrackets tail 0 []
            match reduce bracketedExpression env with
            | Number a ->
                [reduce (CeilToNumber a :: remaining) env], (env |> Map.toSeq |> dict)
            | _ -> ExecError "Execution Error: Invalid " |> raise
        | "round" ->
            let remaining, bracketedExpression = extractBrackets tail 0 []
            match reduce bracketedExpression env with
            | Number a ->
                [reduce (RoundNum a :: remaining) env], (env |> Map.toSeq |> dict)
            | _ -> ExecError "Execution Error: Invalid " |> raise
        | "abs" ->
            let remaining, bracketedExpression = extractBrackets tail 0 []
            match reduce bracketedExpression env with
            | Number a ->
                [reduce (AbsVal a :: remaining) env], (env |> Map.toSeq |> dict)
            | _ -> ExecError "Execution Error: Invalid " |> raise
        | "xrt" ->
            let remaining, bracketedExpression = extractBrackets tail 0 []
            let strippedExpression = removeBeginningAndEndingParenthesis bracketedExpression
            match countCommaOccurance strippedExpression 0 with
            | 1 ->
                let expression1, expression2 = splitTerminalListBasedOnComma strippedExpression
                [reduce ((reduce (RootToTerminals [reduce expression1 env] (reduce expression2 env |> terminalToNum)) env) :: remaining) env], (env |> Map.toSeq |> dict)
            | _ -> 
                InvalidArgumentError "Function xrt expects two arguments" |> raise
        | _ ->
            if env.ContainsKey a then
                let newEnv, remainingTerminals = setArguments tail Map.empty
                let combinedEnv = Map.fold (fun acc key value -> Map.add key value acc) env newEnv
                
                if closed combinedEnv [Word a] |> fst
                then [reduce ((reduce [Word a] combinedEnv) :: remainingTerminals) env], (env |> Map.toSeq |> dict)
                else ExecError "Execution Error: Assignments given in function call do not close expression." |> raise
            else ExecError "Execution Error: Undefined function" |> raise
    | _ ->
        let isClosed, newEnv = closed env expandedTerminals
        if isClosed
        then [reduce expandedTerminals newEnv], (env |> Map.toSeq |> dict)
        else expandedTerminals, (env |> Map.toSeq |> dict)
        
and evaluateDifferentiates terminals (env : Map<string, terminal list>) out =
    match terminals with
    | Function "differentiate" :: tail ->
        let remaining, bracketedExpression = extractBrackets tail 0 []
        evaluateDifferentiates remaining env ((calculateDifferential bracketedExpression env) @ out)
    | head :: tail -> evaluateDifferentiates tail env (head :: out)
    | _ -> List.rev out
        
and calculateDifferential bracketedExpression (env : Map<string, terminal list>) =
    let assignment, expression = extractExpression bracketedExpression.[1..] []
    if assignment <> []
    then
        if (List.filter (fun x -> x = Comma) assignment).Length = 0
        then
            let newEnv, _ = setArguments assignment env
            if closed newEnv expression |> fst
            then
                [reduce (differentiate expression) newEnv]
            else ExecError "Execution Error: Assignment does not match variable in expression" |> raise
        else ExecError "" |> raise
    else
        differentiate expression |> List.rev 
        
and removeBeginningAndEndingParenthesis (input:terminal list) =
    let removeFirstElementAndReverse = input.[1..] |> List.rev
    removeFirstElementAndReverse.[1..] |> List.rev

and splitTerminalListBasedOnComma (input:terminal list) =
    match List.contains Comma input with
    | true ->
        let commaIndex = List.findIndex (fun x -> x = Comma) input
        let x, y = List.splitAt (commaIndex) input
        let z = y.[1..]
        x, z
    | false -> InvalidArgumentError "Invalid Argument Error: At least two arguments expected" |> raise
    
and countCommaOccurance (terminalList: terminal list) (count:int) =
    match terminalList with
    | terminalHead :: terminalTail ->
        match terminalHead with
        | Comma -> countCommaOccurance terminalTail count+1
        | _ -> countCommaOccurance terminalTail count
    | [] -> count