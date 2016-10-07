// Exercise 04 functional programming

// 3.5
type Solution = float * float * float

let solve = function
    | (0, 0, 0) -> 0
    | (x, 0, 0) -> x
    | (x, y, 0) -> x * y
    | (x, y, z) -> x * y * z

// 5.2
// Solve exercise 4.15 useing list.fold & list.foldback
let testList = [[1;2];[3;4;5]]
let rev = (fun acc element -> [element]@acc)
let reverseList = List.fold (fun acc element -> [List.fold (fun acc' element' -> [element']@acc') [] element]@acc) [] testList
reverseList |> printf "The list is now! %A"

// 5.3
// Solve exercise 4.12 useing list.fold & list.foldback
let sum (p, xs) = 