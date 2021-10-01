// HW5, CPSC 3400, Spring 2021
// Solution by Kyle Fraser

// This program simplifies algebraic expressions using
// symbolic differentiation.

// Function implemented: simplify
// Function modified: test 

// Algebraic expression
type Expression =
    | X
    | Y
    | Const of float
    | Neg of Expression
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression
;;

// Pretty-printer for an algebraic expression
let exprToString expr =

    let rec toStr subexpr enclosingPrecedence =        
        let parenthesize s myPrecedence =
            if myPrecedence <= enclosingPrecedence then s else (sprintf "(%s)" s)

        match subexpr with
        // precedence 0 are x and y literals
        | X -> "x"
        | Y -> "y"

        // precedence 1 is unary negation
        | Neg (Neg v) -> sprintf "-(-%s)" (toStr v 1) // avoid --ex in favor of -(-ex)
        | Neg u -> parenthesize (sprintf "-%s" (toStr u 1)) 1

        // precedence 2 is a constant (this is bumped up to get -(3) instead of -3 for Neg (Const 3.0))
        | Const c -> parenthesize (sprintf "%g" c) 2

        // precedence 3 for multiplicative ops
        | Mul (u, v) -> parenthesize (sprintf "%s * %s" (toStr u 3) (toStr v 3)) 3

        // precedence 4 for additive ops
        | Add (u, v) -> parenthesize (sprintf "%s + %s" (toStr u 4) (toStr v 4)) 4
        | Sub (u, v) -> parenthesize (sprintf "%s - %s" (toStr u 4) (toStr v 4)) 4

    toStr expr 5
;;

// This function is used to simplify algebraic expressions. Pattern
// matching is used for symbolic differentiation. 
let rec simplify expr =
    match expr with
    | Const c -> Const c
    | X -> X
    | Y -> Y
    | Neg (Const c) -> Const (c * -1.0)
    | Neg (Neg (Const c)) -> Const c
    | Neg X -> Neg X
    | Neg (Neg X) -> X
    | Neg Y -> Neg Y
    | Neg (Neg Y) -> Y
    | Add (Const u, Const v) -> Const (u + v)
    | Add (X, Const 0.0) -> X
    | Add (Const 0.0, X) -> X
    | Add (Const 0.0, Y) -> Y
    | Add (Y, Const 0.0) -> Y
    | Add (X, Const c) -> Add (X, Const c)
    | Add (Const c, X) -> Add (Const c, X)
    | Add (Const c, Y) -> Add (Const c, Y)
    | Add (Y, Const c) -> Add (Y, Const c)
    | Add (X, X) -> Add (X, X)
    | Add (Y, Y) -> Add (Y, Y)
    | Add (Const 0.0, e1) -> simplify e1
    | Add (e1, Const 0.0) -> simplify e1
    | Add (Const 0.0, Neg e1) -> simplify (Neg e1)
    | Add (Neg e1, Const 0.0) -> simplify (Neg e1)
    | Sub (Const 0.0, Neg e1) -> simplify e1
    | Sub (Const 0.0, e1) -> simplify (Neg e1)
    | Sub (e1, Const 0.0) -> simplify e1
    | Sub (Neg e1, Const 0.0) -> simplify (Neg e1)
    | Sub (Const u, Const v) -> Const (u - v)
    | Sub (X, Const 0.0) -> X
    | Sub (Const 0.0, X) -> Neg X 
    | Sub (Const 0.0, Y) -> Neg Y
    | Sub (Y, Const 0.0) -> Y
    | Sub (Const 0.0, Neg X) -> X
    | Sub (Const 0.0, Neg Y) -> Y
    | Sub (Neg X, Const 0.0) -> Neg X
    | Sub (Neg Y, Const 0.0) -> Neg Y
    | Sub (X, Const c) -> Sub (X, Const c)
    | Sub (Const c, X) -> Sub (Const c, X)
    | Sub (Const c, Y) -> Sub (Const c, Y)
    | Sub (Y, Const c) -> Sub (Y, Const c)
    | Sub (X, X) -> Const 0.0
    | Sub (Y, Y) -> Const 0.0
    | Sub (e1, e2) when e1 = e2 -> Const 0.0 
    | Mul (Const u, Const v) -> Const (u * v)
    | Mul (X, Const 0.0) -> Const 0.0
    | Mul (Const 0.0, X) -> Const 0.0
    | Mul (Const 0.0, Y) -> Const 0.0
    | Mul (Y, Const 0.0) -> Const 0.0
    | Mul (X, Const 1.0) -> X
    | Mul (Const 1.0, X) -> X
    | Mul (Const 1.0, Y) -> Y
    | Mul (Y, Const 1.0) -> Y
    | Mul (X, Const c) -> Mul (X, Const c)
    | Mul (Const c, X) -> Mul (Const c, X)
    | Mul (Const c, Y) -> Mul (Const c, Y)
    | Mul (Y, Const c) -> Mul (Y, Const c)
    | Mul (Const 0.0, e1) -> Const 0.0
    | Mul (e1, Const 0.0) -> Const 0.0
    | Mul (Neg e1, Const 0.0) -> Const 0.0
    | Mul (Const 0.0, Neg e1) -> Const 0.0
    | Neg (Neg e1) -> simplify e1
    | Neg e1 when (simplify e1) = e1 -> Neg (simplify e1)
    | Neg e1 when (simplify e1) <> e1 -> simplify (Neg (simplify e1))
    | Add (e1, e2) when ((simplify e1) <> e1 || (simplify e2) <> e2) -> simplify (Add (simplify e1, simplify e2))
    | Add (e1, e2) -> Add (simplify e1, simplify e2)
    | Sub (e1, e2) when ((simplify e1) <> e1 || (simplify e2) <> e2) -> simplify (Sub (simplify e1, simplify e2))
    | Sub (e1, e2) -> Sub (simplify e1, simplify e2)
    | Mul (e1, e2) when ((simplify e1) <> e1 || (simplify e2) <> e2) -> simplify (Mul (simplify e1, simplify e2))
    | Mul (e1, e2) -> Mul (simplify e1, simplify e2)
  

;;

;;
// This function Prints test results. 
let testResults =

    // This fucntion allows for the testing of defined test cases. 
    let test expr expected description =
        let actual = simplify expr
        printfn "\n%s" description
        printfn "simplify (%s)" (exprToString expr)
        printfn "got: %s" (exprToString actual)
        if actual <> expected then
            printfn "but expected: %s\nFAILED" (exprToString expected)
            false
        else
            printfn "passed" 
            true
    [
        test (Add (Const 5.0, Const 3.0)) (Const 8.0) "t1 - addition involving two numbers";
        test (Sub (Const 5.0, Const 3.0)) (Const 2.0) "t2 - subtraction involving two numbers";
        test (Mul (Const 5.0, Const 3.0)) (Const 15.0) "t3 - multiplication involving two numbers";
        test (Neg (Const 4.0)) (Const -4.0) "t4 - negation involving a number";
        test (Neg (Const -9.0)) (Const 9.0) "t5 - negation involving a number";
        test (Add (X, Const 0.0)) X "t6 - addition with zero";
        test (Add (Const 0.0, Y)) Y "t7 - addition with zero";
        test (Sub (X, Const 0.0)) X "t8 - subtraction with zero";
        test (Sub (Const 0.0, Y)) (Neg Y) "t9 - subtraction with zero";
        test (Sub (Y, Y)) (Const 0.0) "t10 - subtraction of identical terms";
        test (Mul (X, Const 0.0)) (Const 0.0) "t11 - multiplication with zero";
        test (Mul (Const 0.0, Y)) (Const 0.0) "t12 - multiplication with zero";
        test (Mul (X, Const 1.0)) X "t13 - multiplication with one";
        test (Mul (Const 1.0, Y)) Y "t14 - multiplication with one";
        test (Neg (Neg X)) X "t15 - double negation";
        test (Sub (Mul (Const 1.0, X), Add (X, Const 0.0))) (Const 0.0) "t16 - recursive simplification";
        test (Add (Mul (Const 4.0, Const 3.0), Sub (Const 11.0, Const 5.0))) (Const 18.0) "t17";        
        test (Sub (Sub (Add (X, Const 1.0), Add (X, Const 1.0)), Add (Y, X))) (Neg (Add (Y, X))) "t18";
        test (Sub (Const 0.0, Neg (Mul (Const 1.0, X)))) X "t19";
        test (Mul (Add (X, Const 1.0), Neg (Sub (Mul (Const 2.0, Y), X)))) (Mul (Add (X, Const 1.0), Neg (Sub (Mul (Const 2.0, Y), X)))) "t20"
        test (Sub (Const 0.0, Add (Y, X))) (Neg (Add (Y, X))) "t21";
        test (Sub (Add (X, Const 1.0), Add (X, Const 1.0))) (Const 0.0) "t22";
        test (Add (Y, X)) (Add (Y, X)) "t23";
        test (Sub (Const 0.0, Neg X)) (X) "t24";
        test (Neg (Neg (Neg (Neg (Neg (Mul (Const 37.0, Const 3.0))))))) (Const -111.0) "t25";
        test (Mul (Mul (Add (Const 4.0, Const 1.0), Const 0.0), Sub (Const 0.0, Y))) (Const 0.0) "t26";
        test (Add (Sub (Const 1.0, Y), X)) (Add (Sub (Const 1.0, Y), X)) "t27";
        test (Neg (Mul (X, Add (Const 2.0, Const 2.0)))) (Neg (Mul (X, Const 4.0))) "t28"
        test (Sub (Add (Const 2.0, Mul (Const 0.0, X)), Const 0.0)) (Const 2.0) "t29"
        test (Sub (Add (Const 5.0, Const 5.0), Const 4.0)) (Const 6.0) "t30";
        test (Sub (Add (X, Const 1.0), Neg (Sub (Mul (Const 2.0, Y), X)))) (Sub (Add (X, Const 1.0), Neg (Sub (Mul (Const 2.0, Y), X)))) "t31";
        test (Add (Const 0.0, Mul (X, Y))) (Mul (X, Y)) "t32";
        test (Sub (Sub (X, X), (Add (Mul (Y, X), Add (Const 95.0, Const 16.0))))) (Neg (Add (Mul (Y, X), Const 111.0))) "t33";
        test (Add (Neg (Const 18.0), Neg (Const 36.0))) (Const -54.0) "t34";
        test (Mul (Neg X, Sub (Y, Y))) (Const 0.0) "t35";
        test (Add (Const 1.0, Mul (X, Add (Y, Y)))) (Add (Const 1.0, Mul (X, Add (Y, Y)))) "t36";
        test (Sub (Neg (Sub (X, Y)), Neg (Add (X, Y)))) (Sub (Neg (Sub (X, Y)), Neg (Add (X, Y)))) "t37";
        test (Mul (Sub (Const 10.0, Const 100.0), Sub (Const 100.0, Const 10.0))) (Const -8100.0) "t38";
        test (Sub (Add (X, X), Mul (Const 0.0, Sub (X, Y)))) (Add (X, X)) "t39";
        test (Sub (Neg X, Neg Y)) (Sub (Neg X, Neg Y)) "t40";
        test (Mul (Sub (Add (Const 75.0, Const 75.0), Add (Const 2.0, Const 2.0)), Sub (Const -62.0, Const -1000.0))) (Const 136948.0) "t41";
        test (Add (Sub (Mul (Const 42.0, Const 42.0), Mul (Const 42.0, Const 42.0)), Const 42.0)) (Const 42.0) "t42";    

    ];;

let passes = (List.filter (fun bool -> bool) testResults).Length;;
let failures = testResults.Length - passes;;
printfn "%s" (if failures > 0 then (sprintf "%d FAILURES!" failures) else "all tests passed");;
