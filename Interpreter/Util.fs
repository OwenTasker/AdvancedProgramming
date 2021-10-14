module Interpreter.Util

type terminal =
    | Plus
    | Times
    | Divide
    | Minus
    | Exponent
    | Lpar
    | Rpar
    | Decimal
    | Equals
    | Word of string
    | Float of float

exception Parseerror
exception Scanerror
    
// https://stackoverflow.com/questions/42253284/f-check-if-a-string-contains-only-number
let strContainsOnlyNumber (s:string) = System.Double.TryParse s |> fst