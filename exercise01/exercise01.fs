//Test function
let number = 1+1
printfn "The number of the test function is: %i" number

//1.1
let addFour (x : int) = x + 4
let result = addFour 4
printfn "add four to four: %i" result

//1.2
let squareRoot (x : float) (y : float) =
  sqrt(x) + sqrt(y)
let result1 = squareRoot 2.0 2.0
printfn "squareRoot is: %f" result1

let result2 = squareRoot 2.2 3.2
printfn "squareRoot is: %f" result2

//1.3
let res = (fun (addFourExpression : int) ->
  addFourExpression + 4) 4
printfn "addFourExpression: %i" res

let res1 = (fun (squareRootX : float) (squareRootY : float) ->
  sqrt(squareRootX) + sqrt(squareRootY)) 2.0 2.0
printfn "addFourExpression: %f" res1

//1.4
let rec recursiveCounter = function
  | 0 -> 0
  | x -> recursiveCounter(x - 1) + x

let recursiveResult = recursiveCounter 4
printfn "recursiveCounter result: %i" recursiveResult

//1.5
let rec fibonacci = function
  | 0 -> 0
  | 1 -> 1
  | n -> fibonacci(n - 1) + fibonacci(n - 2);;

let fibonacciRes = fibonacci 6
printfn "fibonacci result: %i" fibonacciRes

//1.6
let rec sum = function
  | (m,0) -> m
  | (m,n) -> sum((m + (n - 1) + (m + n)), (n - 1))

let sumRes = sum(6, 4)
printfn "sum is: %i" sumRes

//2.2
let rec stringFun = function
  | (s, 0) -> ""
  | (s, n) -> s + stringFun(s,n-1)

let tests = stringFun("s", 5)
printfn "test: %s" tests

let myString = String.replicate 5 "string"
printfn "string: %s" myString

//4.22
// An infix function +. for adding polynomials.
// Has the same preceedence as +.
//let rec (+.) p q =
//    match (p,q) with
//       | ([],[])            -> 0
//      // | ()            -> ...
//       | (a::p',b::q')  -> a + b && (+.) p q

// multiply P(x) by x
let multX p = p * 4 ;;

// multiply a polynimial by a constant
let rec multC = function
  |  [] -> 0
  |  head::tail -> multX head + multC tail

// An infix function *. for mytiplying polynomials
// Has the same preceedence as *
//let rec ( *.) p q =
//   match p with
//    | ...   -> ...
//    | ...   -> ...  ;;


// convert a polynomial to a string representation
// you may use an auxiliary function
//let toString p = ... ;;

// examples
let p1 = [1; 2; 3];;
let resMultC = multC p1
printfn "The new list %d" resMultC

//let p2 = [1; 2];;
//
//let p3 = [1; 2; 3; 4];
//
//let p4 = p1 +. p2 *. p3;;
//
//let st = toString p4;;
