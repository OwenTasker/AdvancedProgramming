module Lexer.Parser

module Parser =
    
    type terminal =
        | Plus
        | Times
        | Lpar
        | Rpar
        | Int of int

    exception Parseerror
    
    let rec expression tokens = (term >> expressionP) tokens

    and expressionP tokens =
        match tokens with
        | Plus :: tokensTail -> (term >> expressionP) tokensTail
        | _ -> tokens

    and term tokens = (factor >> termP) tokens

    and termP tokens =
        match tokens with
        | Times :: tokensTail -> (factor >> termP) tokensTail
        | _ -> tokens

    and factor tokens =
        match tokens with
        | Int i :: tokensTail -> tokensTail
        | Lpar :: tokensTail ->
            match expression tokensTail with
            | Rpar :: tokensTail -> tokensTail
            | _ -> raise Parseerror
        | _ -> raise Parseerror
