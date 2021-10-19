module Interpreter.Exec

open Interpreter.Util

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
        
let performOperation oplist (numlist : float list) =
    match oplist with
    | [] -> oplist, numlist
    | Lpar :: _ -> oplist, numlist
    | UnaryMinus :: tail
    | UnaryPlus :: tail ->
        let operator = oplist.[0]
        let operand = numlist.[0]
        if numlist.Length > 0 then
            tail, ((unary operator operand) :: numlist.[1 .. ])
        else tail, (unary operator operand) :: []
    | head :: tail ->
        let operand1 = numlist.[0]
        let operand2 = numlist.[1]
        if numlist.Length > 1 then
            tail, ((calculate head operand2 operand1) :: numlist.[2 .. ])
        else tail, ((calculate head operand2 operand1) :: [])
        
let rec evaluateBrackets oplist (numlist : float list) =
    match oplist with
    | [] -> oplist, numlist
    | Lpar :: tail -> tail, numlist
    | _ ->
        let results = performOperation oplist numlist
        match results with
        | oplist, numlist ->
            evaluateBrackets oplist numlist

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

let rec reduceRecursive tokens oplist numlist =
    match tokens with
    | tokenHead :: tokenTail ->
        match tokenHead with
        | Float f ->
            reduceRecursive tokenTail oplist (f :: numlist)
        | Lpar ->
            reduceRecursive tokenTail (tokenHead :: oplist) numlist
        | Rpar ->
            let results = evaluateBrackets oplist numlist
            match results with
            oplist, numlist ->
                reduceRecursive tokenTail oplist numlist
        | UnaryMinus
        | UnaryPlus  
        | Divide
        | Times
        | Minus
        | Plus
        | Exponent ->
            match oplist with
            | [] ->
                reduceRecursive tokenTail (tokenHead :: oplist) numlist
            | opHead :: _ ->
                match opHead with
                | Lpar ->
                    reduceRecursive tokenTail (tokenHead :: oplist) numlist
                | _ ->
                    if (getPrecedence tokenHead < getPrecedence opHead
                        || getPrecedence tokenHead = getPrecedence opHead && getAssociativity tokenHead = "l") then
                        let results = performOperation oplist numlist
                        match results with
                        oplist, numlist ->
                            reduceRecursive tokens oplist numlist
                    else reduceRecursive tokenTail (tokenHead :: oplist) numlist     
        | _ -> raise ExecError
    | [] ->
        match oplist with
        | [] -> numlist.[0]
        | _ ->
            let results = performOperation oplist numlist
            match results with
            | oplist, numlist -> reduceRecursive tokens oplist numlist
            
let reduce tokens =
    reduceRecursive tokens [] []
