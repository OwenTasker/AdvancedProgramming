module Interpreter.Tests.UtilTests.strContainsOnlyNumberTests

open NUnit.Framework
open Interpreter.Util

[<TestFixture>]
type StrContainsOnlyNumberTests () =
        
    [<Test>]
    member this.GivenString_WhenPassedValidNumberWithNoDecimal_ReturnTrue() =
        Assert.True(strContainsOnlyNumber "1")

    [<Test>]
    member this.GivenString_WhenPassedValidNumberWithDecimal_ReturnTrue() =
        Assert.True(strContainsOnlyNumber "1.0")
        
    [<Test>]
    member this.GivenString_WhenPassedInvalidValue_ReturnFalse() =
        Assert.False(strContainsOnlyNumber "a")