// Part exercise 6.2


// Part exercise 6.8
type Instruction = | ADD | SUB | MULT | DIV | SIN | COS | LOG | EXP | PUSH of float
type Stack = float list

let rec createNewStack newSt oldSt =
    printf "the new stack is %A , the old stack is %A" newSt oldSt 

let intpInstr st ins = function
    | ADD -> createNewStack st st
    | _ -> printf "Something else"

let list1 = [ 1; 2; 3; 4; 5; 6 ]

intpInstr list1 ADD

// Properties
printfn "list1.IsEmpty is %b" (list1.IsEmpty)
printfn "list1.Length is %d" (list1.Length)
printfn "list1.Head is %d" (list1.Head)
printfn "list1.Tail.Head is %d" (list1.Tail.Head)
printfn "list1.Tail.Tail.Head is %d" (list1.Tail.Tail.Head)
printfn "list1.Item(1) is %d" (list1.Item(1))
printfn "list1 is %A" list1

let valuesList = [ ("a", 1); ("b", 2); ("c", 3) ]

let resultPick = List.pick (fun elem ->
                    match elem with
                    | (value, 2) -> Some value
                    | _ -> None) valuesList
printfn "%A \n" resultPick
printf "%A \n" valuesList
