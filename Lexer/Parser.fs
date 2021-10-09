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
        | x :: xr ->
            match x with
            | "+" -> scan xr (Plus :: output)
            | "*" -> scan xr (Times :: output)
            | "(" -> scan xr (Lpar :: output)
            | ")" -> scan xr (Rpar :: output)
            | _ ->
                if strContainsOnlyNumber x then scan xr (Int(System.Int32.Parse x) :: output) else failwith "invalid token"
    
    // BNF defined recursively
    // expression ::= term expression'
    let rec expression tokens = (term >> expressionP) tokens

    // expression' ::= + term expression' | empty
    and expressionP tokens =
        match tokens with
        | Plus :: tokensTail -> (term >> expressionP) tokensTail
        | _ -> tokens
    
    // term ::= factor term'
    and term tokens = (factor >> termP) tokens

    // term' ::= * factor term' | empty
    and termP tokens =
        match tokens with
        | Times :: tokensTail -> (factor >> termP) tokensTail
        | _ -> tokens

    // factor ::= int | ( expression )
    // Here the and keyword has allowed us to define the function with mutual recursion - an expression contains a
    // factor and a factor contains an expression
    and factor tokens =
        match tokens with
        | Int _ :: tokensTail -> tokensTail
        | Lpar :: tokensTail ->
            match expression tokensTail with
            | Rpar :: tokensTail -> tokensTail
            | _ -> raise Parseerror
        | _ -> raise Parseerror
