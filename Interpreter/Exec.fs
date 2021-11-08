/// <summary>
/// Module containing methods for execution of commands passed to the MyMathsPal Interpreter.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
///
module Interpreter.Exec

open Interpreter.Util

// http://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/

/// <summary>
/// Performs a binary operation given a terminal representing plus, minus, times, divide, or exponent
/// and two operands as Number terminals.
/// </summary>
///
/// <param name="operator">A terminal representing the operator.</param>
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
        | 0.0 -> raise (CalculateError "Calculate Error: Divide by zero, this operation is undefined.")
        | _ -> Number (op1 / op2)
    | Exponent -> Number (op1 ** op2)
    | _ -> raise (CalculateError "Calculate Error: Invalid operator passed.")

/// <summary>
/// Performs a unary arithmetic operation given terminals representing a unary operator and an operand.
/// </summary>
/// 
/// <param name="operator">A terminal representing the operator.</param>
/// <param name="operand">A Number terminal containing the operand.</param>
///
/// <returns>A Number containing the result of the operation.</returns>
let performUnaryOperation operator operand =
    match operator with
    | UnaryMinus -> Number -operand
    | UnaryPlus -> Number operand
    | _ -> raise (UnaryError "Unary Error: Invalid operator passed.")

/// <summary>
/// Reads the top of the operator stack and passes said operator and necessary operands from the number stack to
/// the correct handling method. Prepends the result to the number stack.
/// </summary>
/// 
/// <param name="opList">A stack of terminals representing operators.</param>
/// <param name="numList">A stack of Number terminals.</param>
///
/// <returns>
/// A tuple containing the tail of the operator stack, and the number stack with the result of the popped operation
/// prepended to replace used operands.
/// </returns>
let performOperation opList numList =
    match opList with
    | []
    | Rpar :: _ 
    | Lpar :: _ -> raise (ExecError "Execution Error: Parenthesis encountered as an operator.")
    | UnaryMinus :: tail
    | UnaryPlus :: tail ->
        let operator = opList.[0]
        match numList with
        | [] -> raise (ExecError "Execution Error: Unary operation called without any remaining operands.")
        | [ Number f; ] ->
            tail, (performUnaryOperation operator f) :: []
        | _ ->
            match numList.[0] with
            | Number f ->
                tail, ((performUnaryOperation operator f) :: numList.[1 .. ])
            | _ -> raise (ExecError "Execution Error: Number stack contains non-number tokens.")
    | head :: tail ->
        match numList with
        | [] -> raise (ExecError "Execution Error: Binary operation called without any operands.")
        | [ Number _; ] -> raise (ExecError "Execution Error: Binary operation called with only one operand.")
        | [ Number f; Number g; ] ->
            let operand1 = f
            let operand2 = g
            tail, ((performBinaryOperation head operand2 operand1) :: [])
        | _ ->
            match numList.[0], numList.[1] with
            | Number f, Number g ->
                let operand1 = f
                let operand2 = g
                tail, ((performBinaryOperation head operand2 operand1) :: numList.[2 .. ])
            | _ -> raise (ExecError "Execution Error: Number stack contains non-number tokens.")

/// <summary>
/// Recursively calls perform operation until a terminal representing a left parenthesis is encountered.
/// </summary>
/// 
/// <param name="opList"></param>
/// <param name="numList"></param>
///
/// <returns></returns>
let rec evaluateBrackets opList numList =
    match opList with
    | []
    | Rpar :: _ -> raise (ExecError "Execution Error: Parenthesis encountered as an operator.")
    | Lpar :: tail ->
        match numList with
        | _ :: _ -> tail, numList
        | [] -> raise (ExecError "Execution Error: Empty number stack following evaluation of bracketed expression.")
    | _ ->
        let results = performOperation opList numList
        match results with
        | opList, numList ->
            evaluateBrackets opList numList

/// <summary>
/// Map containing the precedence and associativity of operators accepted by the performUnaryOperation and
/// performBinaryOperation functions.
/// </summary>
let precedenceAssociativityMap =
    Map [(UnaryMinus, (4, "r"))
         (UnaryPlus, (4, "r"))
         (Exponent, (3, "r"))
         (Times, (2, "l"))
         (Divide, (2, "l"))
         (Plus, (1, "l"))
         (Minus, (1, "l"))]

/// <summary>
/// Retrieves the precedence for an operator from the map.
/// </summary>
/// 
/// <param name="operator"></param>
///
/// <returns></returns>
let getPrecedence operator =
    (Map.find operator precedenceAssociativityMap) |> fst

/// <summary>
/// Retrieves the associativity for an operator from the map.
/// </summary>
///
/// <param name="operator"></param>
///
/// <returns></returns>
let getAssociativity operator =
    (Map.find operator precedenceAssociativityMap) |> snd

/// <summary>
/// Recursively performs the Dijkstra's Shunting Yard algorithm by reading a terminal list representing an infix
/// expression into an operator stack and a number stack. Performs calculations depending on precedence and
/// associativity rather than producing a reverse Polish notation output.
/// </summary>
///
/// <param name="tokens"></param>
/// <param name="opList"></param>
/// <param name="numList"></param>
/// <param name="env"></param>
///
/// <returns></returns>
let rec reduceRecursive tokens opList numList (env: Map<string, terminal list>) =
    match tokens with
    | tokenHead :: tokenTail ->
        match tokenHead with
        | Number _ ->
            reduceRecursive tokenTail opList (tokenHead :: numList) env
        | Word x ->
            if env.ContainsKey x then
                let value = reduce env.[x] env
                reduceRecursive tokenTail opList (value :: numList) env
            else raise (ExecError "Execution Error: Expression with unbound variables passed to reduce.")
        | Lpar ->
            reduceRecursive tokenTail (tokenHead :: opList) numList env
        | Rpar ->
            let results = evaluateBrackets opList numList
            match results with
            opList, numList ->
                reduceRecursive tokenTail opList numList env
        | UnaryMinus
        | UnaryPlus  
        | Divide
        | Times
        | Minus
        | Plus
        | Exponent ->
            match opList with
            | [] ->
                reduceRecursive tokenTail (tokenHead :: opList) numList env
            | opHead :: _ ->
                match opHead with
                | Lpar ->
                    reduceRecursive tokenTail (tokenHead :: opList) numList env
                | _ ->
                    if (getPrecedence tokenHead < getPrecedence opHead
                        || getPrecedence tokenHead = getPrecedence opHead && getAssociativity tokenHead = "l") then
                        let results = performOperation opList numList
                        match results with
                        opList, numList ->
                            reduceRecursive tokens opList numList env
                    else reduceRecursive tokenTail (tokenHead :: opList) numList env
        | _ -> raise (ExecError "Execution Error: Invalid terminal passed to reduce.")
    | [] ->
        match opList with
        | [] ->
            match numList with
            | [ _ ] -> numList.[0]
            | _ -> raise (ExecError "Execution Error: Reduction did not result in exactly one terminal in the number
                          stack")
        | Lpar :: _ -> raise (ExecError "Execution Error: Unmatched left parenthesis.")
        | _ ->
            let results = performOperation opList numList
            match results with
            | opList, numList -> reduceRecursive tokens opList numList env

/// <summary>
/// Wrapper for reduceRecursive to call it with empty operator and number stacks.
/// </summary>
///
/// <param name="tokens"></param>
/// <param name="env"></param>
///
/// <returns></returns>
and reduce tokens (env: Map<string, terminal list>) =
    reduceRecursive tokens [] [] env

/// <summary>
/// Checks whether a function contains any variables whose values are not defined in the current environment.
/// </summary>
///
/// <param name="terminals"></param>
/// <param name="env"></param>
///
/// <returns></returns>
let rec closed terminals (env: Map<string, terminal list>) =
    match terminals with
    | [] -> true
    | Word x :: tail ->
        if env.ContainsKey x && closed env.[x] env then closed tail env else false
    | _ :: tail -> closed tail env

/// <summary>
/// Reads a list of terminals, prepending them to an output list, up to a Comma or Rpar terminal.
/// </summary>
///
/// <param name="inList"></param>
/// <param name="outList"></param>
///
/// <returns></returns>
let rec createTerminalListUpToComma inList outList =
    match inList with
    | Rpar :: _ -> (inList, List.rev outList)
    | Comma :: tail -> (tail, List.rev outList)
    | any :: tail -> createTerminalListUpToComma tail (any :: outList)
    | [] -> raise (ExecError "Execution Error: Function call missing right parenthesis.")

/// <summary>
/// Creates an environment from a list of terminals representing Comma separate assignments.
/// </summary>
///
/// <param name="terminals"></param>
/// <param name="env"></param>
///
/// <returns></returns>
let rec setArguments terminals (env: Map<string, terminal list>) =
    match terminals with
    | Rpar :: _ ->
        env
    | Word x :: Assign :: tail ->
        match createTerminalListUpToComma tail [] with
        | a, b -> setArguments a (env.Add(x, [reduce b env] ))
    | _ -> raise (ExecError "Execution Error: Function call contains non-assignment expression.")

/// <summary>
/// Computes a result, as a terminal list, and an updated execution environment given a terminal list representing
/// a valid statement and an execution environment.
/// </summary>
///
/// <param name="terminals"></param>
/// <param name="env"></param>
///
/// <returns></returns>
let exec terminals (env: Map<string, terminal list>) =
    match terminals with
    | Word x :: Assign :: tail ->
        if closed tail env then 
            let result = [reduce tail env]
        //https://stackoverflow.com/questions/27109142/f-map-to-c-sharp-dictionary/27109303
            result, (env.Add(x, result) |> Map.toSeq |> dict)
        else terminals, (env.Add(x, tail) |> Map.toSeq |> dict) 
    | Word x :: Lpar :: tail ->
        //https://stackoverflow.com/questions/3974758/in-f-how-do-you-merge-2-collections-map-instances
        let newEnv = setArguments tail Map.empty
        let combinedEnv = Map.fold (fun acc key value -> Map.add key value acc) env newEnv
        if closed [Word x] combinedEnv then 
            [reduce [Word x] combinedEnv], (env |> Map.toSeq |> dict)
        else raise (ExecError "Execution Error: Assignments given in function call do not close expression.")
    | _ ->
        if closed terminals env then
            [reduce terminals env], (env |> Map.toSeq |> dict)
        else terminals, (env |> Map.toSeq |> dict)