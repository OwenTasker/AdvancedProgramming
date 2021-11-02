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
    | Assign
    | Function of string
    | Word of string
    | Number of float

exception ParseError of string
exception ScanError of string
exception TokenizeError of string
exception CalculateError
exception UnaryError
exception ExecError

let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
let alphabet = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";
                "n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                "A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";
                "N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z"]

let functions = [
                 ("ceil", "One Argument; A function to determine the ceiling of a decimal value, given a value of 2.1, will return 3")
                 ("floor", "One Argument; A function to determine the floor of a decimal value, given a value of 2.1, will return 2")
                 ("sqrt", "One Argument; A function to determine the square root of a value, given a value of 4, will return 2")
                 ("cbrt", "One Argument; A function to determine the cube root of a value, given a value of 8, will return 2")
                 ("round", "One Argument; A function to determine the rounded value of the provided argument, given a value of 2.5, will return 3")
                 ("plot", "")
                 ]

let functionRegexString =
    let functionRegex = [
        for (x,_) in functions -> "(^" + x + "$)|"
    ]
    let generateRegex = (String.concat "" functionRegex)
    generateRegex.Remove(generateRegex.Length-1)
    
let symbolRegexString =
    let symbols = ["+";"*";"-";"^";"/";"(";")";">"]
    let symbolRegex = [
        for i in symbols -> "(^\\" + i + "$)|"
    ]
    let generateRegex = (String.concat "" symbolRegex)
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

//https://gist.github.com/theburningmonk/3363893
let inline toMap kvps =
    kvps
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

let rec terminalListToString str list =
    match list with
    | Plus :: tail -> terminalListToString (str + "+" ) tail
    | Minus :: tail -> terminalListToString (str + "-" ) tail
    | Times :: tail -> terminalListToString (str + "*" ) tail
    | Divide :: tail -> terminalListToString (str + "/" ) tail
    | Exponent :: tail -> terminalListToString (str + "^" ) tail
    | Lpar :: tail -> terminalListToString (str + "(" ) tail
    | Rpar :: tail -> terminalListToString (str + ")" ) tail
    | UnaryPlus :: tail -> terminalListToString (str + "+" ) tail
    | UnaryMinus :: tail -> terminalListToString (str + "-" ) tail
    | Number x :: tail -> terminalListToString (str + string x ) tail
    | Word x :: tail -> terminalListToString (str + x ) tail
    | Assign :: tail -> terminalListToString (str + "->") tail
    | Equals :: tail -> terminalListToString (str + " = ") tail
    | [] -> str
    | _ -> failwith "shouldn't happen don't want to think about proper error right now it's 8:35am but more importantly it's only October and I'm so bored"
    
