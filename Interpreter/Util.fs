module Interpreter.Util

    
    type terminal =
        | Plus
        | Times
        | Lpar
        | Rpar
        | Int of int

    exception Parseerror
    exception Scanerror
    
    // https://stackoverflow.com/questions/42253284/f-check-if-a-string-contains-only-number
    let strContainsOnlyNumber (s:string) = System.Int32.TryParse s |> fst
    