module Interpreter.Tests.UtilTests

open NUnit.Framework
open Interpreter.Util

[<TestFixture>]
type UtilTests () =
        
    [<Test>]
    member this.GivenString_WhenPassedValidNumberWithNoDecimal_ReturnTrue() =
        Assert.True(strContainsOnlyNumber "1")

    [<Test>]
    member this.GivenString_WhenPassedValidNumberWithDecimal_ReturnTrue() =
        Assert.True(strContainsOnlyNumber "1.0")
        
    [<Test>]
    member this.GivenString_WhenPassedInvalidValue_ReturnFalse() =
        Assert.False(strContainsOnlyNumber "a")
         
    [<Test>]
    member this.GivenString_WhenPassedEmptyValue_ReturnFalse() =
        Assert.False(strContainsOnlyNumber "")
    
    