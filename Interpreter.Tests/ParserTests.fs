module Lexer.Tests.ParserTests

open NUnit.Framework
open Interpreter
open Interpreter.Parser

[<TestFixture>]
type ParserTests () =
        
    [<Test>]
    member this.GivenExpression_WhenPassedInvalidExpression_RaiseParseerror() =
        Assert.Throws<Util.Parseerror>(fun () -> Parser.expression [Util.terminal.Times; Util.terminal.Plus;] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidAddition_ReturnEmptyArray() =
        let result = Parser.expression [Util.terminal.Int 1; Util.terminal.Plus; Util.terminal.Int 5;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidMultiplication_ReturnEmptyArray() =
        let result = Parser.expression [Util.terminal.Int 1; Util.terminal.Times; Util.terminal.Int 5;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedValidNestedExpression_ReturnEmptyArray() =
        let result = Parser.expression [Util.terminal.Int 1; Util.terminal.Times; Util.terminal.Int 5; Util.terminal.Times; Util.terminal.Lpar; Util.terminal.Int 5; Util.terminal.Plus; Util.terminal.Int 6; Util.terminal.Rpar;]
        Assert.That(result, Is.EqualTo([]))
        
    [<Test>]
    member this.GivenExpression_WhenPassedLeftBracketWithoutRight_RaiseParseError() =
        Assert.Throws<Util.Parseerror>(fun () -> Parser.expression [Util.terminal.Lpar; Util.terminal.Int 1; Util.terminal.Plus; Util.terminal.Int 1] |> ignore) |> ignore
        
    [<Test>]
    member this.GivenExpression_WhenPassedRightBracketWithoutLeft_RaiseParseError() =
        Assert.Throws<Util.Parseerror>(fun () -> Parser.expression [Util.terminal.Lpar; Util.terminal.Int 1; Util.terminal.Plus; Util.terminal.Int 1] |> ignore) |> ignore