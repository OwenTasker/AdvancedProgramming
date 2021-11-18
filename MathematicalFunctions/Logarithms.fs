open System

exception InvalidArgumentException of string
exception DomainException of string

//Reference :: https://www.efunda.com/math/taylor_series/logarithmic.cfm
let rec LogE (input:float) (increment:float) (sum:float) =
    match increment with
    | 200.0 -> sum
    | _  -> LogE input (increment+1.0) (sum+(1.0/increment)*((input-1.0)/input)**increment)

let LogE' input = 
    LogE input 1.0 0.0
    
let formNewBaseRuleFraction numerator denominator =
    LogE' numerator / LogE' denominator

let Log2 input =
    formNewBaseRuleFraction input 2.0
    
let Log10 input =
    formNewBaseRuleFraction input 10.0
    
let LogX newBase input =
    formNewBaseRuleFraction input newBase
    
Console.WriteLine (LogE' 10.0)
Console.WriteLine (Log2 10.0)
Console.WriteLine (Log10 10.0)
Console.WriteLine (LogX 3.0 10.0)