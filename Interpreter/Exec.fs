module Interpreter.Exec

open Interpreter.Util

// http://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/

let calculate operator op1 op2 =
    match operator with
    | Plus -> Number (op1 + op2)
    | Minus -> Number (op1 - op2)
    | Times -> Number (op1 * op2)
    | Divide ->
        match op2 with
        | 0.0 -> raise CalculateError
        | _ -> Number (op1 / op2)
    | Exponent -> Number (op1 ** op2)
    | _ -> raise CalculateError
    
let unary operator operand =
    match operator with
    | UnaryMinus -> Number -operand
    | UnaryPlus -> Number operand
    | _ -> raise UnaryError
        
let performOperation oplist numlist =
    match oplist with
    | []
    | Rpar :: _ 
    | Lpar :: _ -> raise ExecError
    | UnaryMinus :: tail
    | UnaryPlus :: tail ->
        let operator = oplist.[0]
        match numlist with
        | [] -> raise ExecError
        | [ Number f; ] ->
            tail, (unary operator f) :: []
        | _ ->
            match numlist.[0] with
            | Number f ->
                tail, ((unary operator f) :: numlist.[1 .. ])
            | _ -> raise ExecError
    | head :: tail ->
        match numlist with
        | [] -> raise ExecError
        | [ Number _; ] -> raise ExecError
        | [ Number f; Number g; ] ->
            let operand1 = f
            let operand2 = g
            tail, ((calculate head operand2 operand1) :: [])
        | _ ->
            match numlist.[0], numlist.[1] with
            | Number f, Number g ->
                let operand1 = f
                let operand2 = g
                tail, ((calculate head operand2 operand1) :: numlist.[2 .. ])
            | _ -> raise ExecError

        
let rec evaluateBrackets oplist numlist =
    match oplist with
    | []
    | Rpar :: _ -> raise ExecError
    | Lpar :: tail ->
        match numlist with
        | _ :: _ -> tail, numlist
        | [] -> raise ExecError
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

let rec reduceRecursive tokens oplist numlist (env: Map<string, terminal list>) =
    match tokens with
    | tokenHead :: tokenTail ->
        match tokenHead with
        | Number _ ->
            reduceRecursive tokenTail oplist (tokenHead :: numlist) env
        | Word x ->
            if env.ContainsKey x then
                let value = env.[x].[0]
                reduceRecursive tokenTail oplist (value :: numlist) env
            else raise ExecError
        | Lpar ->
            reduceRecursive tokenTail (tokenHead :: oplist) numlist env
        | Rpar ->
            let results = evaluateBrackets oplist numlist
            match results with
            oplist, numlist ->
                reduceRecursive tokenTail oplist numlist env
        | UnaryMinus
        | UnaryPlus  
        | Divide
        | Times
        | Minus
        | Plus
        | Exponent ->
            match oplist with
            | [] ->
                reduceRecursive tokenTail (tokenHead :: oplist) numlist env
            | opHead :: _ ->
                match opHead with
                | Lpar ->
                    reduceRecursive tokenTail (tokenHead :: oplist) numlist env
                | _ ->
                    if (getPrecedence tokenHead < getPrecedence opHead
                        || getPrecedence tokenHead = getPrecedence opHead && getAssociativity tokenHead = "l") then
                        let results = performOperation oplist numlist
                        match results with
                        oplist, numlist ->
                            reduceRecursive tokens oplist numlist env
                    else reduceRecursive tokenTail (tokenHead :: oplist) numlist env
        | _ -> raise ExecError
    | [] ->
        match oplist with
        | [] ->
            match numlist with
            | [ _ ] -> numlist.[0]
            | _ -> raise ExecError
        | Lpar :: _ -> raise ExecError
        | _ ->
            let results = performOperation oplist numlist
            match results with
            | oplist, numlist -> reduceRecursive tokens oplist numlist env
            
let reduce tokens (env: Map<string, terminal list>) =
    reduceRecursive tokens [] [] env
   
let rec closed terminals (env: Map<string, terminal list>) =
    match terminals with
    | [] -> true
    | Word x :: tail ->
        if env.ContainsKey x && closed env.[x] env then closed tail env else false
    | _ :: tail -> closed tail env
    
let exec terminals (env: Map<string, terminal list>) =
    match terminals with
    | Word x :: Assign :: tail ->
        if closed tail env then 
            let result = [reduce tail env]
        //https://stackoverflow.com/questions/27109142/f-map-to-c-sharp-dictionary/27109303
            result, (env.Add(x, result) |> Map.toSeq |> dict)
        else terminals, (env.Add(x, tail) |> Map.toSeq |> dict) 
    | _ ->
        if closed terminals env then
            [reduce terminals env], (env |> Map.toSeq |> dict)
        else terminals, (env |> Map.toSeq |> dict)