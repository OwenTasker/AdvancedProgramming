/// <summary>
/// Module containing all externally available functions within the F# implementation for the MyMathsPal Application
/// </summary>
///
/// <namespacedoc>
///     <summary>Interpreter</summary>
/// </namespacedoc>
module public Interpreter.PublicInterface

open Interpreter.Lexer
open Interpreter.Parser
open Interpreter.Exec

/// <summary>
/// Interpret function, takes in a string list and an environment of user defined variables and turns it into
/// a terminal list representing a valid MMP expression
/// </summary>
///
/// <param name="input">A string list represting an input, this will ultimately be turned into a terminal list</param>
/// <param name="env">
/// A tuple of strings and terminal lists, this contains all user defined functions that currently
/// exist in the program
/// </param>
/// 
/// <returns>Returns a valid MMP expression</returns>
let public interpret input env =
    lexer input |> parse |> exec env

/// <summary>
/// Closed function, takes in a string list and an environment of user defined variables and turns it into a boolean
/// expression as to whether each variable within an expression is defined or not.
/// </summary>
///
/// <param name="input">A string list represting an input, this will ultimately be turned into a terminal list</param>
/// <param name="env">
/// A tuple of strings and terminal lists, this contains all user defined functions that currently
/// exist in the program
/// </param>
/// 
/// <returns>Returns a boolean based on whether each variable within an expression is already defined or not</returns>
let public closed input env =
    lexer input |> closed env

/// <summary>
/// StringToTerminalList function, takes in a string list and runs it through the lexer and parser
/// </summary>
///
/// <param name="input">A string list represting an input, this will ultimately be turned into a terminal list</param>
/// 
/// <returns>Returns a valid MMP expression</returns>
let public stringToTerminalList input =
    lexer input |> parse

/// <summary>
/// TerminalListToString function, takes in a terminal list and converts it to a string
/// </summary>
///
/// <param name="input">A terminal list representing an input, will be ultimately transformed into a string</param>
/// 
/// <returns>Returns a string representation of a terminal list</returns>
let public terminalListToString input =
    Util.terminalListToString "" input

/// <summary>
/// ToMap function, takes in a sequence of key-value pairs and converts it into a C# map
/// </summary>
///
/// <param name="kvps">A sequence of key-value pairs that will ultimately be turned into a map for use in C#</param>
/// 
/// <returns>Returns a map representation of a sequence of key-value pairs</returns>
let public toMap kvps =
    Util.toMap kvps