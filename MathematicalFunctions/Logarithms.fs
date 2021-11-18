open System

exception InvalidArgumentException of string
exception DomainException of string

//Reference :: https://www.efunda.com/math/taylor_series/logarithmic.cfm
let rec LogErecursive (input:float) (increment:float) (sum:float) =
    match increment with
    | 200.0 -> sum
    | _  -> LogErecursive input (increment+1.0) (sum+(1.0/increment)*((input-1.0)/input)**increment)

let LogE input = 
    LogErecursive input 1.0 0.0