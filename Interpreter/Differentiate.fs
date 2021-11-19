/// <summary>
/// Module containing functions for differentiation of expressions. Interprets values as dual numbers which are defined
/// such that operations thereupon result in a derivative being calculated.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module Interpreter.Differentiate

open Interpreter.Util

/// <summary>
/// Type defining dual numbers as being of the form a + be where e^2 = 0. Operations are redefined under this type
/// such that the e term gives the derivative of the real term.
/// </summary>
///
/// <remarks>
/// http://www.ams.org/publicoutreach/feature-column/fc-2017-12#:~:text=Dual%20numbers%20and%20the%20forward,%3D0%20%CF%B5%202%20%3D%200%20.
/// https://www.infoq.com/presentations/automatic-differentiation/
/// </remarks>
/// 
type Dual =
    | Const of real : terminal * epsilon : float
    | Var of real : terminal * epsilon : float
    | Expr of real : terminal list *  epsilon : terminal list
    
    /// <summary>Sums two Duals according to the identity (a + be) + (c + de) = (a + c) + (b + d)e</summary>
    /// 
    /// <param name="operand1">The left hand operand, corresponding to a + be</param>
    /// <param name="operand2">The right hand operand, corresponding to c + de</param>
    /// 
    /// <returns>The result of addition of two Duals (a + c) + (b + d)e</returns>
    static member (+) (operand1 : Dual, operand2 : Dual) =
        match operand1, operand2 with
        | Var (a, _), Const (c, _)
        | Const (a, _), Var (c, _) -> Expr ([a; Plus; c;],
                                           [Number 1.0])
        | Var (a, _), Var (c, _) -> Expr ([a; Plus; c;],
                                          [Number 2.0])
        | Const (a, _), Const (c, _) -> Expr ([a; Plus; c;],
                                              [Number 0.0])
        | Var (a, b), Expr (c, d) -> Expr (a :: Plus :: c,
                                           Number b :: Plus :: d)
        | Expr (a, b), Var (c, d) -> Expr (a @ Plus :: [c],
                                           b @ Plus :: [Number d])
        | Const (a, _), Expr (c, d) -> Expr (a :: Plus :: c,
                                             d)
        | Expr (a, b), Const (c, _) -> Expr (a @ Plus :: [c],
                                             b)
        | Expr (a, b), Expr (c, d) -> Expr (a @ Plus :: c,
                                            b @ Plus :: d)
    
    /// <summary>
    /// Subtracts one Dual from another according to the identity (a + be) - (c + de) = (a - c) + (b - d)e
    /// </summary>
    /// 
    /// <param name="operand1">The left hand operand, corresponding to a + be</param>
    /// <param name="operand2">The right hand operand, corresponding to c + de</param>
    /// 
    /// <returns>The result of subtraction of two Duals (a - c) + (b - d)e</returns>
    static member (-) (operand1 : Dual, operand2 : Dual) =
        match operand1, operand2 with
        | Var (a, _), Const (c,_)
        | Const (a, _), Var (c,_) -> Expr ([a; Minus; c;],
                                           [Number 1.0])
        | Var (a, _), Var (c, _) -> Expr ([a; Minus; c;],
                                          [Number 2.0])
        | Const (a, _), Const (c, _) -> Expr ([a; Minus; c;],
                                              [Number 0.0])
        | Var (a, b), Expr (c, d) -> Expr (a :: Minus :: c,
                                           Number b :: Minus :: d)
        | Expr (a, b), Var (c, d) -> Expr (a @ Minus :: [c],
                                           b @ Minus :: [Number d])
        | Const (a, _), Expr (c, d) -> Expr (a :: Minus :: c,
                                             d)
        | Expr (a, b), Const (c, _) -> Expr (a @ Minus :: [c],
                                             b)
        | Expr (a, b), Expr (c, d) -> Expr (a @ Minus :: c,
                                            b @ Minus :: d)
        
    // (a + be)(c + de) = ac + (ad + bc)e
    /// <summary>Multiplies two Duals according to the identity (a + be)(c + de) = ac + (ad + bc)e</summary>
    /// 
    /// <param name="operand1">The left hand operand, corresponding to a + be</param>
    /// <param name="operand2">The right hand operand, corresponding to c + de</param>
    /// 
    /// <returns>The result of multiplication of two Duals ac + (ad + bc)e</returns>
    static member (*) (operand1 : Dual, operand2 : Dual) =
        match operand1, operand2 with
        | Var (a, _), Const (c,_) -> Expr ([a; Times; c;],
                                           [c])
        | Const (a, _), Var (c,_) -> Expr ([a; Times; c;],
                                           [a])
        | Var (a, _), Var (c, _) -> Expr ([a; Times; c;],
                                          [a; Plus; c])
        | Const (a, _), Const (c, _) -> Expr ([a; Times; c;],
                                              [Number 0.0])
        | Var (a, _), Expr (c, d) -> Expr (a :: Times :: Lpar :: c @ [Rpar],
                                           a :: Times :: Lpar ::  d @ Rpar :: Plus :: c)
        | Expr (a, b), Var (c, _) -> Expr (Lpar :: a @ Rpar :: Times :: [c],
                                           a @ Plus :: Lpar :: b @ Rpar :: Times :: [c])
        | Const (a, _), Expr (c, d) -> Expr (a :: Times :: Lpar :: c @ [Rpar],
                                             a :: Times :: Lpar :: d @ [Rpar])
        | Expr (a, b), Const (c, _) -> Expr (Lpar :: a @ Rpar :: Times :: [c],
                                             Lpar :: b @ Rpar :: Times :: [c])
        | Expr (a, b), Expr (c, d) -> Expr (Lpar :: a @ Rpar :: Times :: Lpar :: c @ [Rpar],
                                            Lpar :: a @ Rpar :: Times :: Lpar :: d @ Rpar :: Plus :: Lpar :: b @ Rpar :: Times :: Lpar :: c @ [Rpar])
        
    // (a + be)/(c + de) = a/c + ((bc - ad)/c^2)e
    /// <summary>
    /// Divides one Dual by another according to the identity (a + be)/(c + de) = a/c + ((bc - ad)/c^2)e
    /// </summary>
    /// 
    /// <param name="operand1">The left hand operand, corresponding to a + be</param>
    /// <param name="operand2">The right hand operand, corresponding to c + de</param>
    /// 
    /// <returns>The result of division of two Duals (a + be)/(c + de) = a/c + ((bc - ad)/c^2)e</returns>
    static member (/) (operand1 : Dual, operand2 : Dual) =
        match operand1, operand2 with
        | Var (a, _), Const (c,_) -> Expr ([a; Divide; c;],
                                           [c; Divide; c; Exponent; Number 2.0])
        | Const (a, _), Var (c,_) -> Expr ([a; Divide; c;],
                                           [UnaryMinus; a; Divide; c; Exponent; Number 2.0])
        | Var (a, _), Var (c, _) -> Expr ([a; Divide; c;],
                                          [Lpar; c; Minus; a; Rpar; Divide; c; Exponent; Number 2.0])
        | Const (a, _), Const (c, _) -> Expr ([a; Divide; c;],
                                              [Number 0.0])
        | Var (a, _), Expr (c, d) -> Expr (a :: Divide :: Lpar :: c @ [Rpar],
                                           Lpar :: c @ Minus :: a :: Times :: Lpar :: d @ [Rpar] @ Rpar :: Divide :: Lpar :: c @ [Rpar; Exponent; Number 2.0])
        | Expr (a, b), Var (c, _) -> Expr (Lpar :: a @ [Rpar; Divide; c;],
                                           Lpar :: Lpar :: b @ Rpar :: Times :: c :: Minus :: Lpar :: a @ [Rpar; Rpar; Divide; c; Exponent; Number 2.0])
        | Const (a, _), Expr (c, d) -> Expr (a :: Divide :: Lpar :: c @ [Rpar],
                                             Lpar :: UnaryMinus :: a :: Times :: Lpar :: d @ [Rpar; Rpar; Divide; Lpar;] @ c @ [Rpar; Exponent; Number 2.0])
        | Expr (a, b), Const (c, _) -> Expr (Lpar :: a @ [Rpar; Divide; c;] ,
                                             Lpar :: Lpar :: b @ [Rpar; Times; c; Rpar; Divide; c; Exponent; Number 2.0])
        | Expr (a, b), Expr (c, d) -> Expr (Lpar :: a @ Rpar :: Times :: Lpar :: c @ [Rpar],
                                            Lpar :: Lpar :: b @ [Rpar; Times; Lpar] @ c @ [Rpar; Minus; Lpar] @ a @ [Rpar; Times; Lpar] @ d @ [Rpar; Rpar; Divide; Lpar] @ c @ [Rpar; Exponent; Number 2.0])
        
    // (a + be)^(c + de) = (a^c) + (c*a^(c-1))e
    // THIS IS CURRENTLY INCORRECTLY DEFINED, WHEN A VARIABLE IS AN EXPONENT IT SHOULDN'T FOLLOW THE POWER RULE.
    /// <summary>
    /// Raises one Dual to the power of another according to the identity (a + be) + (c + de) = (a + c) + (b + d)e
    /// </summary>
    /// 
    /// <param name="operand1">The left hand operand, corresponding to a + be</param>
    /// <param name="operand2">The right hand operand, corresponding to c + de</param>
    /// 
    /// <returns>The result of exponentiation of two Duals (a + be)^(c + de) = (a^c) + (c*a^(c-1))e</returns>
    static member Pow (operand1 : Dual, operand2 : Dual) =
        match operand1, operand2 with
        | Var (a, _), Const (c,_) -> Expr ([a; Exponent; c],
                                           [c; Times; a; Exponent; Lpar; c; Minus; Number 1.0; Rpar])
        | Const (a, _), Var (c,_) -> Expr ([a; Exponent; c;],
                                           [c; Times; a; Exponent; Lpar; c; Minus; Number 1.0; Rpar])
        | Var (a, _), Var (c, _) -> Expr ([a; Exponent; c;],
                                          [c; Times; a; Exponent; Lpar; c; Minus; Number 1.0; Rpar])
        | Const (a, _), Const (c, _) -> Expr ([a; Exponent; c;],
                                              [Number 0.0])
        | Var (a, _), Expr (c, _) -> Expr (a :: Exponent :: Lpar :: c @ [Rpar],
                                           Lpar :: c @ [Rpar; Times; a; Exponent; Lpar] @ c @ [Minus; Number 1.0; Rpar])
        | Expr (a, _), Var (c, _) -> Expr (Lpar :: a @ Rpar :: Exponent :: [c],
                                           c :: Times :: a @ [Exponent; Lpar; c; Minus; Number 1.0; Rpar])
        | Const (a, _), Expr (c, _) -> Expr (a :: Exponent :: Lpar :: c @ [Rpar],
                                             Lpar :: c @ [Rpar; Times; a; Exponent; Lpar] @ c @ [Minus; Number 1.0; Rpar])
        | Expr (a, _), Const (c, _) -> Expr (Lpar :: a @ Rpar :: Exponent :: [c],
                                             c :: Times :: a @ [Exponent; Lpar; c; Minus; Number 1.0; Rpar])
        | Expr (a, _), Expr (c, _) -> Expr (Lpar :: a @ Rpar :: Exponent :: Lpar :: c @ [Rpar],
                                            Lpar :: c @ [Rpar; Times;] @ a @ [Exponent; Lpar] @ c @ [Minus; Number 1.0; Rpar])  

/// <summary>
/// Performs a binary operation given a terminal representing plus, minus, times, divide, or exponent
/// and two operands as Duals.
/// </summary>
///
/// <param name="operator">A terminal representing an operator.</param>
/// <param name="op1">A Dual for the left side of the operation.</param>
/// <param name="op2">A Dual for the right side of the operation.</param>
///
/// <returns>A Dual containing the result of the operation.</returns>
let performBinaryOperation operator op1 op2 =
    match operator with
    | Plus -> op1 + op2
    | Minus -> op1 - op2
    | Times -> op1 * op2
    | Divide -> op1 / op2
    | Exponent -> op1 ** op2
    | _ -> CalculateError "Calculate Error: Invalid operator passed." |> raise

/// <summary>
/// Performs a unary arithmetic operation given a terminal a unary operator and an Dual.
/// </summary>
/// 
/// <param name="operator">A terminal representing an operator.</param>
/// <param name="operand">A Dual containing the operand.</param>
///
/// <returns>A Dual containing the result of the operation.</returns>
let performUnaryOperation operator operand =
    match operator with
    | UnaryMinus ->
        match operand with
        | Const (a, b)
        | Var (a, b) -> Expr ([UnaryMinus; Lpar; a; Rpar], [UnaryMinus; Number b;])
        | Expr (a, b) -> Expr (UnaryMinus :: Lpar :: a @ [Rpar], UnaryMinus :: Lpar :: b @ [Rpar])
    | UnaryPlus -> operand
    | _ -> UnaryError "Unary Error: Invalid operator passed." |> raise

/// <summary>
/// Reads the top of the operator stack and passes said operator and necessary operands from the number stack to
/// the correct handling method. Prepends the result to the number stack.
/// </summary>
/// 
/// <param name="operator">A terminal representing an operator.</param>
/// <param name="numStack">A stack of Duals.</param>
///
/// <returns>The number stack with the outcome of the operation prepended.</returns>
let performOperation operator numStack =
    match operator with
    | Rpar 
    | Lpar -> ExecError "Execution Error: Parenthesis encountered as an operator." |> raise
    | UnaryMinus
    | UnaryPlus ->
        match numStack with
        | [] -> ExecError "Execution Error: Unary operation called without any remaining operands." |> raise
        | [ operand; ] -> (performUnaryOperation operator operand) :: []
        | operand :: _ -> (performUnaryOperation operator operand) :: numStack.[1 .. ]
    | _ ->
        match numStack with
        | [] -> ExecError "Execution Error: Binary operation called without any operands." |> raise
        | [ _; ] -> ExecError "Execution Error: Binary operation called with only one operand." |> raise
        | [ operand2; operand1; ] -> (performBinaryOperation operator operand1 operand2) :: []
        | operand2 :: operand1 :: _ -> (performBinaryOperation operator operand1 operand2) :: numStack.[2 .. ]

/// <summary>
/// Recursively calls perform operation until a terminal representing a left parenthesis is encountered.
/// </summary>
/// 
/// <param name="opStack">A stack of terminals representing operators.</param>
/// <param name="numStack">A stack of Number terminals.</param>
///
/// <returns>
/// A tuple containing the operator stack with all elements up to and including the next left parenthesis popped and
/// the number stack with the elements updated to represent the outcome of the bracketed operation.
/// </returns>
let rec evaluateBrackets opStack numStack =
    match opStack with
    | []
    | Rpar :: _ -> ExecError "Execution Error: Parenthesis encountered as an operator." |> raise
    | Lpar :: tail ->
        match numStack with
        | _ :: _ -> tail, numStack
        | [] -> ExecError "Execution Error: Empty number stack following evaluation of bracketed expression." |> raise
    | head :: tail -> evaluateBrackets tail (performOperation head numStack)

/// <summary>
/// Recursively performs the Dijkstra's Shunting Yard algorithm by reading a terminal list representing an infix
/// expression into an operator stack and a number stack. Performs calculations depending on precedence and
/// associativity rather than producing a reverse Polish notation output.
/// </summary>
///
/// <param name="terminals">A queue of terminals representing an expression in infix notation.</param>
/// <param name="opStack">A stack of operator terminals to compute the result of the infix expression.</param>
/// <param name="numStack">A stack of Number terminals to compute the result of the infix expression.</param>
///
/// <returns>A Number terminal containing the outcome of the expression.</returns>
let rec autoDifferentiate terminals opStack numStack =
    match terminals with
    | terminalHead :: terminalTail ->
        match terminalHead with
        | Word _ -> autoDifferentiate terminalTail opStack (Var (terminalHead, 1.0) :: numStack)
        | Number _ -> autoDifferentiate terminalTail opStack (Const (terminalHead, 0.0) :: numStack)
        | Lpar -> autoDifferentiate terminalTail (terminalHead :: opStack) numStack
        | Rpar ->
            match evaluateBrackets opStack numStack with
            opStack, numStack -> autoDifferentiate terminalTail opStack numStack
        | UnaryMinus
        | UnaryPlus  
        | Divide
        | Times
        | Minus
        | Plus
        | Exponent ->
            match opStack with
            | [] -> autoDifferentiate terminalTail (terminalHead :: opStack) numStack
            | opHead :: opTail ->
                match opHead with
                | Lpar -> autoDifferentiate terminalTail (terminalHead :: opStack) numStack
                | _ ->
                    if (getPrecedence terminalHead < getPrecedence opHead
                        || getPrecedence terminalHead = getPrecedence opHead
                           && getAssociativity terminalHead = "l")
                    then autoDifferentiate terminals opTail (performOperation opHead numStack)
                    else autoDifferentiate terminalTail (terminalHead :: opStack) numStack
        | _ -> ExecError "Execution Error: Invalid terminal passed to differentiate." |> raise
    | [] ->
        match opStack with
        | [] ->
            match numStack with
            | [ Var (_, b) ]
            | [ Const (_, b) ] -> [Number b]
            | [ Expr (_, b) ] -> b
            | _ -> ExecError "Execution Error: Differentiation did not result in exactly one terminal in the number
                              stack" |> raise
        | head :: tail -> autoDifferentiate terminals tail (performOperation head numStack)

/// <summary>Wrapper for autoDifferentiate that calls it with empty number and operator stacks</summary>
///
/// <param name="terminals">A list of terminals representing a valid expression for differentiation.</param> 
let differentiate terminals = autoDifferentiate terminals [] []
