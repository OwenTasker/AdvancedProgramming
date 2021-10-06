type TOKEN_DEFINITIONS = 
    | T_PLUS = 1  //+
    | T_MULT = 2  //*
    | T_SUB  = 3  //-
    | T_DIV  = 4  ///
    | T_POW  = 5  //^
    | T_MOD  = 6  //%
    | T_LPAR = 7  //(
    | T_RPAR = 8  //)
    | T_NUM  = 9  //0-9 - This specifically needs a lot of work, currently only capable of storing single numbers 
    | T_WSP  = 10 

module Lexer = 
    
    let arrayOfTokens = Array.zeroCreate 32
    let mutable arrayOfNumbers = List.empty<int>
    let mutable currentPos = 0
    
    let addNumberToList number =
        arrayOfNumbers <- List.append arrayOfNumbers [number]
        
    let addTokenToList (token:TOKEN_DEFINITIONS) =
        if not (token.Equals TOKEN_DEFINITIONS.T_WSP) then
            printfn $"TOKEN %i{currentPos} = ID %i{int token}"
            arrayOfTokens.[currentPos] <- int token
            currentPos <- currentPos+1
        else
            printfn "Whitespace Ignored"
        
    let lexInput inputVal=
        for value in inputVal do
            match value with 
            | '+' -> addTokenToList (TOKEN_DEFINITIONS.T_PLUS)
            | '*' -> addTokenToList (TOKEN_DEFINITIONS.T_MULT)
            | '-' -> addTokenToList (TOKEN_DEFINITIONS.T_SUB)
            | '/' -> addTokenToList (TOKEN_DEFINITIONS.T_DIV)
            | '^' -> addTokenToList (TOKEN_DEFINITIONS.T_POW)
            | '%' -> addTokenToList (TOKEN_DEFINITIONS.T_MOD)
            | '(' -> addTokenToList (TOKEN_DEFINITIONS.T_LPAR)
            | ')' -> addTokenToList (TOKEN_DEFINITIONS.T_RPAR)
            | '1'|'2'|'3'|'4'|'5'
            | '6'|'7'|'8'|'9'|'0' ->
                let inline charToInt c = int c - int '0' //nice little ascii converter
                addTokenToList(TOKEN_DEFINITIONS.T_NUM)
                addNumberToList (charToInt (int value))
            | ' ' -> addTokenToList (TOKEN_DEFINITIONS.T_WSP)
            | _ -> failwith "Undefined Token"

        let listOfVals:int[] = arrayOfTokens |> Array.filter(fun x -> x <> 0)
        listOfVals


[<EntryPoint>]
let main args=
    printf ">> "
    let varToUse = System.Console.ReadLine()
    printfn $"Check Input - %s{varToUse}"
    let finalAtr = Lexer.lexInput varToUse
    printf "%A - %A - %A"  varToUse finalAtr Lexer.arrayOfNumbers
    0
   