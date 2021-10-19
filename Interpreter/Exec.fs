module Interpreter.Exec

open System.Reflection.Emit
open Interpreter.Util
open System.Collections.Generic

// http://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/

let calculate operator op1 op2 =
    match operator with
    | terminal.Plus -> op1 + op2
    | terminal.Minus -> op1 - op2
    | terminal.Times -> op1 * op2
    | terminal.Divide -> op1 / op2
    | terminal.Exponent -> op1 ** op2
    | _ -> raise CalculateError
    
let unary operator operand : float =
    match operator with
    | UnaryMinus -> -operand
    | UnaryPlus -> operand
    | _ -> raise UnaryError
            
let numStack = Stack<float>()
let opStack = Stack<terminal>()

let performOperation operator =
    match operator with
    | UnaryMinus
    | UnaryPlus ->
        let operand = numStack.Pop()
        unary operator operand
    | _ ->
        let op1 = numStack.Pop()
        let op2 = numStack.Pop()
        calculate operator op2 op1

let precedenceAssociativity =
    Map [(UnaryMinus, (4, "r"))
         (UnaryPlus, (4, "r"))
         (Exponent, (3, "r"))
         (Times, (2, "l"))
         (Divide, (2, "l"))
         (Plus, (1, "l"))
         (Minus, (1, "l"))]
    
let getPrecedence operator =
    (Map.find operator precedenceAssociativity) |> fst
    
let getAssociativity operator =
    (Map.find operator precedenceAssociativity) |> snd

let rec reduce tokens =
    match tokens with
    | head :: tail ->
        match head with
        | Float f -> numStack.Push(f)
        | Lpar -> opStack.Push(head)
        | Rpar ->
            while (opStack.Peek()) <> Lpar do
                let operator = opStack.Pop()
                numStack.Push(performOperation operator)
            opStack.Pop() |> ignore
        | UnaryMinus
        | UnaryPlus  
        | Divide
        | Times
        | Minus
        | Plus
        | Exponent ->
            if opStack.Count = 0 then opStack.Push(head)
            else
                let newOpAssoc = getAssociativity head
                let newOpPrec = getPrecedence head
                
                while opStack.Count > 0 && opStack.Peek() <> Lpar &&
                      (newOpPrec < getPrecedence (opStack.Peek())
                       || (newOpPrec = getPrecedence (opStack.Peek()) && newOpAssoc = "l")) do
                    let operator = opStack.Pop()
                    numStack.Push(performOperation operator)
                
                opStack.Push(head)           
        | _ -> raise ExecError
        reduce tail
    | [] ->
        while opStack.Count > 0 do
            let operator = opStack.Pop()
            numStack.Push(performOperation operator)

        numStack.Pop()
