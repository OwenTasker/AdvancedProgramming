module Interpreter.Differentiate

// http://www.ams.org/publicoutreach/feature-column/fc-2017-12#:~:text=Dual%20numbers%20and%20the%20forward,%3D0%20%CF%B5%202%20%3D%200%20.
// https://www.infoq.com/presentations/automatic-differentiation/

open Interpreter.Util

type Dual =
    | Const of x : terminal * eps : float
    | Var of x : terminal * eps : float
    | Expr of x : terminal list *  eps : terminal list
    | Op of terminal
    
    // (a + be) + (c + de) = (a + c) + (b + d)e
    static member (+) (a : Dual, b : Dual) =
        match a, b with
        | Var (a, _), Const (c, _)
        | Const (a, _), Var (c, _) -> Expr ([a; Plus; c;],
                                           [Number 1.0])
        | Var (a, _), Var (c, _) -> Expr ([a; Plus; c;],
                                          [Number 2.0])
        | Const (a, _), Const (c, _) -> Expr ([a; Plus; c;],
                                              [Number 0.0])
        | Var (a, b), Expr (c, d) -> Expr (a :: Plus :: c,
                                           Number b :: Plus :: d)
        | Expr (a, b), Var (c, d) -> Expr (a @ Plus :: [c],
                                           b @ Plus :: [Number d])
        | Const (a, _), Expr (c, d) -> Expr (a :: Plus :: c,
                                             d)
        | Expr (a, b), Const (c, _) -> Expr (a @ Plus :: [c],
                                             b)
        | Expr (a, b), Expr (c, d) -> Expr (a @ Plus :: c,
                                            b @ Plus :: d)
        | _ -> failwith "can't add op"
    
    //(a + be) - (c + de) = (a - c) + (b - d)e
    static member (-) (a : Dual, b : Dual) =
        match a, b with
        | Var (a, _), Const (c,_)
        | Const (a, _), Var (c,_) -> Expr ([a; Minus; c;],
                                           [Number 1.0])
        | Var (a, _), Var (c, _) -> Expr ([a; Minus; c;],
                                          [Number 2.0])
        | Const (a, _), Const (c, _) -> Expr ([a; Minus; c;],
                                              [Number 0.0])
        | Var (a, b), Expr (c, d) -> Expr (a :: Minus :: c,
                                           Number b :: Minus :: d)
        | Expr (a, b), Var (c, d) -> Expr (a @ Minus :: [c],
                                           b @ Minus :: [Number d])
        | Const (a, _), Expr (c, d) -> Expr (a :: Minus :: c,
                                             d)
        | Expr (a, b), Const (c, _) -> Expr (a @ Minus :: [c],
                                             b)
        | Expr (a, b), Expr (c, d) -> Expr (a @ Minus :: c,
                                            b @ Minus :: d)
        | _ -> failwith "can't add op"
        
    // (a + be)(c + de) = ac + (ad + bc)e
    static member (*) (a : Dual, b : Dual) =
        match a, b with
        | Var (a, _), Const (c,_) -> Expr ([a; Times; c;],
                                           [c])
        | Const (a, _), Var (c,_) -> Expr ([a; Times; c;],
                                           [a])
        | Var (a, _), Var (c, _) -> Expr ([a; Times; c;],
                                          [a; Plus; c])
        | Const (a, _), Const (c, _) -> Expr ([a; Times; c;],
                                              [Number 0.0])
        | Var (a, _), Expr (c, d) -> Expr (a :: Times :: Lpar :: c @ [Rpar],
                                           a :: Times :: Lpar ::  d @ Rpar :: Plus :: c)
        | Expr (a, b), Var (c, _) -> Expr (Lpar :: a @ Rpar :: Times :: [c],
                                           a @ Plus :: Lpar :: b @ Rpar :: Times :: [c])
        | Const (a, _), Expr (c, d) -> Expr (a :: Times :: Lpar :: c @ [Rpar],
                                             a :: Times :: Lpar :: d @ [Rpar])
        | Expr (a, b), Const (c, _) -> Expr (Lpar :: a @ Rpar :: Times :: [c],
                                             Lpar :: b @ Rpar :: Times :: [c])
        | Expr (a, b), Expr (c, d) -> Expr (Lpar :: a @ Rpar :: Times :: Lpar :: c @ [Rpar],
                                            Lpar :: a @ Rpar :: Times :: Lpar :: d @ Rpar :: Plus :: Lpar :: b @ Rpar :: Times :: Lpar :: c @ [Rpar])
        | _ -> failwith "can't add op"
        
    // (a + be)/(c + de) = a/c + ((bc - ad)/c^2)e
    static member (/) (a : Dual, b : Dual) =
        match a, b with
        | Var (a, _), Const (c,_) -> Expr ([a; Divide; c;],
                                           [c; Divide; c; Exponent; Number 2.0])
        | Const (a, _), Var (c,_) -> Expr ([a; Divide; c;],
                                           [UnaryMinus; a; Divide; c; Exponent; Number 2.0])
        | Var (a, _), Var (c, _) -> Expr ([a; Divide; c;],
                                          [Lpar; c; Minus; a; Rpar; Divide; c; Exponent; Number 2.0])
        | Const (a, _), Const (c, _) -> Expr ([a; Divide; c;],
                                              [Number 0.0])
        | Var (a, _), Expr (c, d) -> Expr (a :: Divide :: Lpar :: c @ [Rpar],
                                           Lpar :: c @ Minus :: a :: Times :: Lpar :: d @ [Rpar] @ Rpar :: Divide :: Lpar :: c @ [Rpar; Exponent; Number 2.0])
        | Expr (a, b), Var (c, _) -> Expr (Lpar :: a @ [Rpar; Divide; c;],
                                           Lpar :: Lpar :: b @ Rpar :: Times :: c :: Minus :: Lpar :: a @ [Rpar; Rpar; Divide; c; Exponent; Number 2.0])
        | Const (a, _), Expr (c, d) -> Expr (a :: Divide :: Lpar :: c @ [Rpar],
                                             Lpar :: UnaryMinus :: a :: Times :: Lpar :: d @ [Rpar; Rpar; Divide; Lpar;] @ c @ [Rpar; Exponent; Number 2.0])
        | Expr (a, b), Const (c, _) -> Expr (Lpar :: a @ [Rpar; Divide; c;] ,
                                             Lpar :: Lpar :: b @ [Rpar; Times; c; Rpar; Divide; c; Exponent; Number 2.0])
        | Expr (a, b), Expr (c, d) -> Expr (Lpar :: a @ Rpar :: Times :: Lpar :: c @ [Rpar],
                                            Lpar :: Lpar :: b @ [Rpar; Times; Lpar] @ c @ [Rpar; Minus; Lpar] @ a @ [Rpar; Times; Lpar] @ d @ [Rpar; Rpar; Divide; Lpar] @ c @ [Rpar; Exponent; Number 2.0])
        | _ -> failwith "can't add op"
        
    // (a + be)^(c + de) = (a^c) + (c*a^(c-1))e
    static member Pow (a : Dual, b : Dual) =
        match a, b with
        | Var (a, _), Const (c,_) -> Expr ([a; Exponent; c],
                                           [c; Times; a; Exponent; Lpar; c; Minus; Number 1.0; Rpar])
        | Const (a, _), Var (c,_) -> Expr ([a; Exponent; c;],
                                           [c; Times; a; Exponent; Lpar; c; Minus; Number 1.0; Rpar])
        | Var (a, _), Var (c, _) -> Expr ([a; Exponent; c;],
                                          [c; Times; a; Exponent; Lpar; c; Minus; Number 1.0; Rpar])
        | Const (a, _), Const (c, _) -> Expr ([a; Exponent; c;],
                                              [Number 0.0])
        | Var (a, _), Expr (c, _) -> Expr (a :: Exponent :: Lpar :: c @ [Rpar],
                                           Lpar :: c @ [Rpar; Times; a; Exponent; Lpar] @ c @ [Minus; Number 1.0; Rpar])
        | Expr (a, _), Var (c, _) -> Expr (Lpar :: a @ Rpar :: Exponent :: [c],
                                           c :: Times :: Lpar :: a @ [Rpar; Exponent; Lpar; c; Minus; Number 1.0; Rpar])
        | Const (a, _), Expr (c, _) -> Expr (a :: Exponent :: Lpar :: c @ [Rpar],
                                             Lpar :: c @ [Rpar; Times; a; Exponent; Lpar] @ c @ [Minus; Number 1.0; Rpar])
        | Expr (a, _), Const (c, _) -> Expr (Lpar :: a @ Rpar :: Exponent :: [c],
                                             c :: Times :: Lpar :: a @ [Rpar; Exponent; Lpar; c; Minus; Number 1.0; Rpar])
        | Expr (a, _), Expr (c, _) -> Expr (Lpar :: a @ Rpar :: Exponent :: Lpar :: c @ [Rpar],
                                            Lpar :: c @ [Rpar; Times; Lpar] @ a @ [Rpar; Exponent; Lpar] @ c @ [Minus; Number 1.0; Rpar])
        | _ -> failwith "can't add op"    
    
    
let rec replaceTerminals terminals out =
    match terminals with
    | [] -> List. rev out
    | Word x :: tail
    | UnaryPlus :: Word x :: tail ->  replaceTerminals tail (Var ((Word x), 1.0) :: out)
    | Number f :: tail 
    | UnaryPlus :: Number f :: tail -> replaceTerminals tail (Const ((Number f), 0.0) :: out)
    | UnaryMinus :: Word x :: tail ->  replaceTerminals tail (Expr ([UnaryMinus; Word x], [Number 1.0]) :: out)
    | UnaryMinus :: Number f :: tail -> replaceTerminals tail (Expr ([UnaryMinus; Number f], [Number 0.0]) :: out)
    | x :: tail -> replaceTerminals tail (Op x :: out)
    
// Replace all terminals with Duals
// Read Duals
    // Match ops with logic to combine adjacent duals
        // Does order of operations matter?
            // Lets find out
            
let rec autoDiff (duals : Dual list) =
    match duals with
    | [] -> failwith "empty duals"
    | [ a ] ->
        match a with
        | Op _ -> failwith "Loose operator"
        | Var (_, b)
        | Const (_, b) -> [Number b]
        | Expr (_, b) -> b
    | [_; _;] -> failwith "malformed expression"
    | a :: b :: c :: tail ->
        match a, b, c with
        | Var _ , Op operator, Var _ 
        | Var _ , Op operator, Const _
        | Const _ , Op operator, Var _ 
        | Const _ , Op operator, Const _
        | Var _ , Op operator, Expr _ 
        | Expr _ , Op operator, Var _
        | Const _ , Op operator, Expr _ 
        | Expr _ , Op operator, Const _
        | Expr _ , Op operator, Expr _ ->
            match operator with
            | Plus -> autoDiff ((a + c) :: tail)
            | Minus -> autoDiff ((a - c) :: tail)
            | Times -> autoDiff ((a * c) :: tail)
            | Divide -> autoDiff ((a / c) :: tail)
            | Exponent -> autoDiff ((a ** c) :: tail)
            | _ -> failwith "malformed expression"
        | _ -> failwith "malformed expression"
        
let differentiate terminals = autoDiff (replaceTerminals terminals []) 
    