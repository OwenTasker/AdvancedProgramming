module Lexer.Parser

module Parser =
    
    // https://www.itu.dk/~sestoft/parsernotes-fsharp.pdf
    
    type terminal =
        | Plus
        | Times
        | Lpar
        | Rpar
        | Int of int

    exception Parseerror
    
    // https://stackoverflow.com/questions/42253284/f-check-if-a-string-contains-only-number
    let strContainsOnlyNumber (s:string) = System.Int32.TryParse s |> fst
    
    // Scan each token by recursively scanning the list tail. Prepend elements to output and reverse for efficiency.
    let rec scan tokens output  =
        match tokens with
        | [] -> List.rev output
        | tokenHead :: tokensTail ->
            match tokenHead with
            | "+" -> scan tokensTail (Plus :: output)
            | "*" -> scan tokensTail (Times :: output)
            | "(" -> scan tokensTail (Lpar :: output)
            | ")" -> scan tokensTail (Rpar :: output)
            | _ ->
                if strContainsOnlyNumber tokenHead then scan tokensTail (Int(System.Int32.Parse tokenHead) :: output)
                else failwith "invalid token"
    
    // BNF defined recursively
    // expression ::= term expression'
    let rec expression terminals = (term >> expressionP) terminals

    // expression' ::= + term expression' | empty
    and expressionP terminals =
        match terminals with
        | Plus :: terminalsTail -> (term >> expressionP) terminalsTail
        | _ -> terminals
    
    // term ::= factor term'
    and term terminals = (factor >> termP) terminals

    // term' ::= * factor term' | empty
    and termP terminals =
        match terminals with
        | Times :: terminalsTail -> (factor >> termP) terminalsTail
        | _ -> terminals

    // factor ::= int | ( expression )
    // Here the and keyword has allowed us to define the function with mutual recursion - an expression contains a
    // factor and a factor contains an expression
    and factor terminals =
        match terminals with
        | Int _ :: terminalsTail -> terminalsTail
        | Lpar :: terminalsTail ->
            match expression terminalsTail with
            | Rpar :: terminalsTail -> terminalsTail
            | _ -> raise Parseerror
        | _ -> raise Parseerror
