module Interpreter.Util

open System.Text.RegularExpressions

type terminal =
    | Plus
    | Times
    | Divide
    | Minus
    | UnaryPlus
    | UnaryMinus
    | Exponent
    | Lpar
    | Rpar
    | Equals
    | Function of string
    | Word of string
    | Number of float

exception ParseError
exception ScanError
exception TokenizeError
exception CalculateError
exception UnaryError
exception ExecError

let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
let alphabet = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";
                "n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                "A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";
                "N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z"]

let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
let alphabet = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";
                "n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                "A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";
                "N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z"]

let functionRegexString =
    let functions = ["ceil";"floor";"sqrt";"round"]
    let functionRegex = [
        for i in functions -> "(^" + i + "$)|"
    ]
    let generateRegex = (String.concat "" functionRegex)
    generateRegex.Remove(generateRegex.Length-1)
let symbolRegexString =
    let symbols = ["+";"*";"-";"^";"/";"=";"(";")"]
    let functionRegex = [
        for i in symbols -> "[\\" + i + "]|"
    ]
    let generateRegex = (String.concat "" functionRegex)
    generateRegex.Remove(generateRegex.Length-1)

//https://sodocumentation.net/fsharp/topic/962/active-patterns       
let (|NumberMatchLex|_|) (input:string) =
    if Regex.IsMatch(input, "[0-9]+|[.]") then
        Some(input)
    else
        None

let (|NumberMatchScan|_|) (input:string) =
    //https://stackoverflow.com/questions/12643009/regular-expression-for-floating-point-numbers
    if Regex.IsMatch(input, "[+-]?([0-9]*[.])?[0-9]+") then
        Some(input)
    else
        None

let (|AlphabetMatch|_|) (input:string)  =
    if Regex.IsMatch(input, "[a-zA-Z]+") then
        Some(input)
    else
        None

let (|SymbolMatch|_|) (input:string)  =
    if Regex.IsMatch(input, symbolRegexString) then
        Some(input)
    else
        None

let (|FunctionMatch|_|) (input:string) =
    if Regex.IsMatch(input, functionRegexString) then
        Some(input)
    else
        None

    
// https://stackoverflow.com/questions/42253284/f-check-if-a-string-contains-only-number
let strContainsOnlyNumber (s:string) = System.Double.TryParse s |> fst