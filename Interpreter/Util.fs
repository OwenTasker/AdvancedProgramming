/// <summary>
/// Module containing helper functions used by various areas of the MyMathsPal Interpreter.
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module public Interpreter.Util

open System.Text.RegularExpressions

/// <summary>Exception thrown when an error is encountered while parsing a list of terminals.</summary>
exception public ParseError of string
/// <summary>Exception thrown when an error is encountered while scanning a list of tokens.</summary>
exception public ScanError of string
/// <summary>Exception thrown when an error is encountered while tokenizing a list of strings.</summary>
exception public TokenizeError of string
/// <summary>Exception thrown when an error is encountered while computing a binary operation.</summary>
exception public CalculateError of string
/// <summary>Exception thrown when an error is encountered while computing a unary operation.</summary>
exception public UnaryError of string
/// <summary>Exception thrown when an error is encountered while executing an expression.</summary>
exception public ExecError of string
/// <summary>Exception thrown when an error is encountered while executing a function, used specifically for enforcing
/// more stringent parameters</summary>
exception public InvalidArgumentError of string

/// <summary>A type representing terminal characters accepted by the Interpreter.</summary>
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
    | Comma
    | Function of string
    | Word of string
    | Number of float

/// <summary>List of valid predefined functions in the Interpreter.</summary>
let public functions = [
                 ("ceil", "One Argument; A function to determine the ceiling of a decimal value, given a value of 2.1, will return 3")
                 ("floor", "One Argument; A function to determine the floor of a decimal value, given a value of 2.1, will return 2")
                 ("sqrt", "One Argument; A function to determine the square root of a value, given a value of 4, will return 2")
                 ("cbrt", "One Argument; A function to determine the cube root of a value, given a value of 8, will return 2")
                 ("round", "One Argument; A function to determine the rounded value of the provided argument, given a value of 2.5, will return 3")
                 ("plot", "One or Three arguments; Plot to graph the first argument, limits determined by second and third arguments")
                 ("ln", "One Argument; A function to determine the natural logarithm of the provided argument to 6 accurate decimal points")
                 ("logTwo", "One Argument; A function to determine the base 2 logarithm of the provided argument to 6 accurate decimal points")
                 ("logTen", "One Argument; A function to determine the base 10 logarithm of the provided argument to 6 accurate decimal points")
                 ("logX", "Two Arguments; A function to determine the base X logarithm of the second provided argument to 6 accurate decimal points")
                 ("differentiate", "One Argument; A function to differentiate an expression provided as an argument, given a value of x^2, will return 2*x")
                 ("integrate", "Three Arguments; A function to estimate the area beneath a graph between given bounds")
                 ("abs", "One Argument; A function to determine the absolute value of an expression, given a value of -12, returns 12")
                 ("xrt", "Two Arguments; determines the xth root of a given value")
                 ("gcd", "Two Arguments; Calculates the greatest common divisor of two integer values")
                 ("mod", "Two Arguments; Calculates the modulo value of a pair of floating point values")
                 ("rand", "Two Arguments; Returns a random integer based on a specified lower and upper bound")
                 ("clear", "Zero Arguments; Clears the console and user-defined variables")
                 ("pi", "Zero Arguments: Constant variable PI")
                 ("euler", "Zero Arguments: Constant variable Eulers Number")
                 ]

/// <summary>Regex string for matching predefined functions.</summary>
let private functionRegexString =
    let functionRegex = [
        for x,_ in functions -> "(^" + x + "$)|"
    ]
    let generateRegex = (String.concat "" functionRegex)
    generateRegex.Remove(generateRegex.Length-1)

/// <summary>Function to match functions passed to the passed to tokenize or scan.</summary>
///
/// <param name="input">A string to query for its composition.</param>
///
/// <returns>An Option containing the input string if it was a predefined function, or None otherwise.</returns>
let internal (|FunctionMatch|_|) (input:string) =
    if Regex.IsMatch(input, functionRegexString) then
        Some(input)
    else
        None

//https://gist.github.com/theburningmonk/3363893
/// <summary>Function to convert a C# Dictionary to an F# Map.</summary>
let inline internal toMap kvps =
    kvps
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

/// <summary>Function to convert a terminal to its string representation.</summary>
///
/// <param name="x">A terminal represented by the terminal type.</param>
///
/// <returns>A string representation of the terminal.</returns>
let internal individualTerminalToString x =
    match x with
    | Times -> "*"
    | Divide -> "/"
    | Plus
    | UnaryPlus -> "+"
    | Minus
    | UnaryMinus -> "-"
    | Exponent -> "^"
    | Lpar -> "("
    | Rpar -> ")"
    | Assign -> "->"
    | Comma -> ","
    | Function func -> string func
    | Word word -> string word
    | Number num -> string num

/// <summary>Function to convert a list of terminals to a string.</summary>
///
/// <param name="str">A string containing the as yet converted terminals.</param>
/// <param name="list">A list of terminals to convert to a string.</param>
///
/// <returns>A string representation of the terminal list.</returns>
let rec internal terminalListToString str list =
    match list with
    | head :: tail -> terminalListToString (str + individualTerminalToString head ) tail
    | [] -> str

/// <summary>
/// Function to convert an individual Number terminal into a floating point number
/// </summary>
///
/// <param name="term">A terminal value</param>
///
/// <returns>A string representation of the terminal list.</returns>
///
/// <exception cref="InvalidArgumentError">Thrown when the input value is not of Number terminal type</exception>
let internal terminalToNum term =
    match term with
    | Number _ -> term |> individualTerminalToString |> System.Double.Parse
    | _ -> InvalidArgumentError "Cannot Parse Non Number Terminal As Number" |> raise

/// <summary>
/// Map containing the precedence and associativity of operators accepted by the performUnaryOperation and
/// performBinaryOperation functions.
/// </summary>
let internal precedenceAssociativityMap =
    Map [(UnaryMinus, (4, "r"))
         (UnaryPlus, (4, "r"))
         (Exponent, (3, "r"))
         (Times, (2, "l"))
         (Divide, (2, "l"))
         (Plus, (1, "l"))
         (Minus, (1, "l"))]

/// <summary>
/// Retrieves the precedence for an operator from the map.
/// </summary>
///
/// <param name="operator">A terminal representing an operator.</param>
///
/// <returns>The precedence value of the operator.</returns>
let internal getPrecedence operator =
    (Map.find operator precedenceAssociativityMap) |> fst

/// <summary>
/// Retrieves the associativity for an operator from the map.
/// </summary>
///
/// <param name="operator">A terminal representing an operator.</param>
///
/// <returns>The associativity value of the operator.</returns>
let internal getAssociativity operator =
    (Map.find operator precedenceAssociativityMap) |> snd

/// <summary>
/// Recursively calls perform operation until a terminal representing a left parenthesis is encountered.
/// </summary>
///
/// <param name="opStack">A stack of terminals representing operators.</param>
/// <param name="numStack">A stack of Number terminals.</param>
/// <param name="performOperation">A function to control how operations are performed over the numbers</param>
///
/// <returns>
/// A tuple containing the operator stack with all elements up to and including the next left parenthesis popped and
/// the number stack with the elements updated to represent the outcome of the bracketed operation.
/// </returns>
let rec internal evaluateBrackets opStack (numStack : 'a list) (performOperation : terminal -> 'a list -> 'a list) =
    match opStack with
    | []
    | Rpar :: _ -> ExecError "Execution Error: Parenthesis encountered as an operator." |> raise
    | Lpar :: tail ->
        match numStack with
        | _ :: _ -> tail, numStack
        | [] -> ExecError "Execution Error: Empty number stack following evaluation of bracketed expression." |> raise
    | head :: tail -> evaluateBrackets tail (performOperation head numStack) performOperation

let rec checkUniqueVariables terminals (vars: Set<string>) =
    match terminals with
    | head :: tail ->
        match head with
        | Word a -> checkUniqueVariables tail (vars.Add a)
        | _ -> checkUniqueVariables tail vars
    | [] -> vars.Count > 1