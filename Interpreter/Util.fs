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
    | Function of string
    | Word of string
    | Float of float

exception ParseError
exception ScanError
exception TokenizeError
    
// https://stackoverflow.com/questions/42253284/f-check-if-a-string-contains-only-number
let strContainsOnlyNumber (s:string) = System.Double.TryParse s |> fst