// Exercise 02

// 2.1
let devideFunc = function
    | n when n % 2 = 0 && n % 5 <> 0 -> true
    | n when n % 3 = 0 && n % 5 <> 0 -> true
    | _ -> false

devideFunc 24
    |> printfn "The devision result is: %b"

// 2.6
let notDevisible = function
    | (d, n) when n % d = 0 -> false
    | (d, n) when n % d <> 0 -> true
    | _ -> false

notDevisible (2, 5)
    |> printfn "The devision result with (2, 5), should be true: %b"

notDevisible (3, 9)
    |> printfn "The devision result with (3, 9), should be false: %b"

printf "\n"
printf "\n"

// 2.7
let rec test = function
    | (a, b, c) when a > b -> notDevisible (b, c)
    | (a, b, c) when a <= b ->
                notDevisible (a, c)
                |> printfn "not devisible result %b" 
                test (a + 1, b, c)
    | (_, _, _) -> false

test (1, 10, 2) 
    |> printf "Wooooohhhhpaaaa, this is working awesome!!! %b"

printf "\n"
printf "\n"

// 2.8 Pascal's triangle
let coefficients = function
    | (_, 0) -> 1
    | (n, k) when n <> 0 && k <> 0 && n > k -> ((n - 1) / (k - 1)) + ((n - 1) / k)
    | _ -> 0

coefficients (2, 0)
    |> printf "Pascal's triangle!!! This will return 1, %i"

printf "\n"
printf "\n"

let rec primeTest = function
    | (a, b, c) when a <= b -> notDevisible (a, c) && primeTest (a + 1, b, c)
    | (a, b, c) -> true;

let prime n = primeTest (2, n - 1, n)

// Prime number generator exercise v1
let rec primeNumbers = function
    | (n, m, primeList) when n = m -> primeList
    | (n, m, primeList) ->
        if prime n
            then primeNumbers (n + 1, m, primeList@[n])
            else primeNumbers (n + 1, m, primeList)

primeNumbers (20, 50, [])
    |> printf "Prime numbers between 20 and 50 are: %A"

printf "\n"
printf "\n"

// Prime number generator exercise v2

let rec primeNumbersTo' = function
    | (n, m, primeList) when m <= 0 -> []
    | (0, m, primeList) -> primeNumbersTo'(1, m, primeList)
    | (1, m, primeList) -> primeNumbersTo'(2, m, primeList)
    | (n, m, primeList) when n = m -> primeList
    | (n, m, primeList) when prime n -> primeNumbersTo'(n + 1, m, primeList@[n])
    | (n, m, primeList) -> primeNumbersTo'(n + 1, m, primeList)

let firstPrimes m = primeNumbersTo' (0, m, [])
    
firstPrimes 50
    |> printf "Prime numbers to 50 is: %A"