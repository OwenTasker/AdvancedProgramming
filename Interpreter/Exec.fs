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
        
let performOperation opList numList =
    match opList with
    | []
    | Rpar :: _ 
    | Lpar :: _ -> raise ExecError
    | UnaryMinus :: tail
    | UnaryPlus :: tail ->
        let operator = opList.[0]
        match numList with
        | [] -> raise ExecError
        | [ Number f; ] ->
            tail, (unary operator f) :: []
        | _ ->
            match numList.[0] with
            | Number f ->
                tail, ((unary operator f) :: numList.[1 .. ])
            | _ -> raise ExecError
    | head :: tail ->
        match numList with
        | [] -> raise ExecError
        | [ Number _; ] -> raise ExecError
        | [ Number f; Number g; ] ->
            let operand1 = f
            let operand2 = g
            tail, ((calculate head operand2 operand1) :: [])
        | _ ->
            match numList.[0], numList.[1] with
            | Number f, Number g ->
                let operand1 = f
                let operand2 = g
                tail, ((calculate head operand2 operand1) :: numList.[2 .. ])
            | _ -> raise ExecError

        
let rec evaluateBrackets opList numList =
    match opList with
    | []
    | Rpar :: _ -> raise ExecError
    | Lpar :: tail ->
        match numList with
        | _ :: _ -> tail, numList
        | [] -> raise ExecError
    | _ ->
        let results = performOperation opList numList
        match results with
        | opList, numList ->
            evaluateBrackets opList numList

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
            else raise ExecError
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
        | _ -> raise ExecError
    | [] ->
        match opList with
        | [] ->
            match numList with
            | [ _ ] -> numList.[0]
            | _ -> raise ExecError
        | Lpar :: _ -> raise ExecError
        | _ ->
            let results = performOperation opList numList
            match results with
            | opList, numList -> reduceRecursive tokens opList numList env
            
and reduce tokens (env: Map<string, terminal list>) =
    reduceRecursive tokens [] [] env
   
let rec closed terminals (env: Map<string, terminal list>) =
    match terminals with
    | [] -> true
    | Word x :: tail ->
        if env.ContainsKey x && closed env.[x] env then closed tail env else false
    | _ :: tail -> closed tail env
    
let rec createTerminalListUpToComma inList outList =
    match inList with
    | Rpar :: _ -> (inList, outList)
    | Comma :: tail -> (tail, outList)
    | any :: tail -> createTerminalListUpToComma tail (any :: outList)
    | [] -> raise ExecError
    
let rec setArguments terminals (env: Map<string, terminal list>) =
    match terminals with
    | Rpar :: _ ->
        env
    | Word x :: Assign :: tail ->
        match createTerminalListUpToComma tail [] with
        | a, b -> setArguments a (env.Add(x, [reduce b env] ))
    | _ -> raise ExecError
    
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
        else raise ExecError
    | _ ->
        if closed terminals env then
            [reduce terminals env], (env |> Map.toSeq |> dict)
        else terminals, (env |> Map.toSeq |> dict)