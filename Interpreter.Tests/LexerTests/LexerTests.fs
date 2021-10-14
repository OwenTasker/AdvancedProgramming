module Interpreter.Tests.LexerTests.LexerTests

open NUnit.Framework
open Interpreter.Lexer
open Interpreter.Util


[<TestFixture>]
type LexerTests ()=
    
    [<Test>]
    member this.GivenLexer_WhenPassedEmptyExpression_ReturnTupleOfEmptyLists() =
        let result = lexer [""]
        Assert.That(result, Is.EqualTo(([],[])))
        
    [<Test>]
    member this.GivenLexer_WhenPassedValidExpression_ReturnCorrectTuple() =
        let result = lexer ["1";"0";"+";"1"]
        Assert.That(result, Is.EqualTo((["10";"+";"1"],[Float 10.0; Plus; Float 1.0])))
        
    [<Test>]
    member this.GivenLexer_WhenPassedInvalidExpression_ThrowTokenizeError() =
        Assert.Throws<TokenizeError>(fun _ -> lexer["?"] |> ignore) |> ignore
        