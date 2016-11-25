// ---------------------------------------------------------------
// Mandatory assignment 4
// Course: Functional programming - 02157
// ---------------------------------------------------------------
// Student info:
// Name: Anders Holmgaard Opstrup
// Student no. s160148
// ---------------------------------------------------------------
// Collaborator info:
// Name: Thomas Juel Neergaard
// Student no. s155991
// ---------------------------------------------------------------

// Part 1.
type Prop = 
    | P of string
    | Dis of Prop * Prop
    | Con of Prop * Prop
    | Neg of Prop

// Part 2.
let propositionConverter = function
    | Dis (x, y) -> Dis (x, y)
    | Con (x, y) -> Con (x, y)
    | Neg x ->
        match x with
        | Dis (x, y) -> Con(Neg x, Neg y) 
        | Con (x, y) -> Dis(Neg x, Neg y)
        | Neg x -> x
        | _ -> x
    | x -> x

// Part 3.
let literal = function
    | Con (x, Dis (y, z)) -> Dis(Con(x, y), Con(x, z))
    | Con (Dis (y, z), x) -> Dis(Con(x, y), Con(x, z))
    | x -> x

// Part 4.
let rec litOf = function
    | P s -> Set.singleton s
    | Con (x, y) -> Set.union (litOf x) (litOf y)
    | Neg x -> litOf x
    | Dis (x, y) -> Set.union (litOf x) (litOf y)
    //| _ -> Set.empty

let dnfToSet = function 
    | Dis (x, y) -> Set.union (litOf x) (litOf y)
    | x -> litOf x

// Part 5.
let isConsistent s =
    Set.fold (fun acc el ->
        match el with
        | Neg(e)    -> (not (Set.contains e s)) && acc
        | P(e)      -> (not (Set.contains (Neg(P(e))) s)) && acc
        | _         -> acc
    ) true s

let removeInconsistent dns =
    Set.filter isConsistent dns

// Part 6.
let toDNFsets = function
    | Con (x, y)    -> litOf (Con (x, y))
    | Dis (x, y)    -> dnfToSet (Dis (x, y))
    | Neg x         -> litOf x
    | P (a)         -> litOf (P (a))

let impl = function
    | (P (a), P (b))        -> true
    | (P (a), Neg (b))      -> false
    | (Neg (a), P (b))      -> true
    | (Neg (a), Neg (b))    -> true
    | _                     -> false

let iff = function
    | (P (a), P (b))        -> true
    | (P (a), Neg (b))      -> false
    | (Neg (b), P (a))      -> false
    | (Neg (a), Neg (b))    -> true
    | _                     -> false

// Part 7.
//What kinds of citizens are a, b and c?
//Answer: a is a knave, b is a knight and c is a knave. 
//The reasoning behind this is: We know a is a knave because he says everyone is a knave,
//which can't be true since there has to be at least one knight, and he can't be a knight, 
//because then he would be a knave himself. b cannot be a knave either, since that would result in 
//0, 2 or 3 knight, which we know is also wrong. We also know there can't be 0 knights, because A is lying
//and we know it's not 2 or 3 knights, because we know a is lying and also says b is lying. 
//This means b is the only knight and therefore c must be knave as well.

//propositional logic
let aStatement = Con (Con (Neg (P "a"), Neg (P "b")), Neg (P "c"))
let bStatement = Dis(Dis(Con (Con (P "a", Neg (P "b")), Neg (P "c")), Con (Con (Neg (P "a"), P "b"), Neg (P "c"))), Con (Con (Neg (P "a"), Neg (P "b")), P "c"))
let finalStatement = Dis(aStatement, bStatement)

toDNFsets (finalStatement)
    |> printf "Result of the riddle with knights and knaves: %A \n"

// Part 8.
let rec badProp = function
    | (0, prop) -> prop
    | (n, prop) -> badProp (n-1, Con (Con (Dis (P "A", P "B" ), Dis (P "A", P "B" )), prop))

// ----------------------- TESTS ----------------------- // 
let atomA = P "a"
let atomB = P "b"
let atomC = P "c"
let negA = Neg atomA
let AandB = Con(atomA, atomB)
let AorB = Dis(atomA, atomB)
let CandAorB = Con(atomC, AorB)
let AorBandC = Con(AorB, atomC)
let notAandB = Neg AandB
let notAorB = Neg AorB

let consistentSet = (Set.empty.Add(atomA).Add(atomB).Add(atomC))
let inconsistentSet = (Set.empty.Add(atomA).Add(atomB).Add(atomC).Add(negA))

propositionConverter negA
    |> printf "The result of propositionConverter (not a) is: %A \n"

propositionConverter notAandB
    |> printf "The result of propositionConverter (not (a and b)) is: %A \n"

propositionConverter notAorB
    |> printf "The result of propositionConverter (not (a or b)) is: %A \n"

literal CandAorB
    |> printf "The result of literal (C and A or B) is: %A \n"

literal AorBandC
    |> printf "The result of literal (A or B and C) is: %A \n"

litOf (AandB)
    |> printf "The result of litOf (expected result [a, b]) is: %A \n"

litOf (Con (P "A", Neg (Con (P "B", Con (P "A", P "C"))))) 
    |> printfn "The result of litOf (expected result [A, B, C]) is: %A \n"

dnfToSet (Dis (P "A", Neg (Con (P "B", Con (P "A", P "C"))))) 
    |> printfn "The result of dnfToSet is: %A \n"

isConsistent (Set.empty.Add(atomA).Add(atomB).Add(atomC))
    |> printf "The result of isConsistent (expected result true) is: %A \n"

isConsistent (Set.empty.Add(atomA).Add(atomB).Add(atomC).Add(negA))
    |> printf "The result of removeInconsistent (expected result false) is: %A \n"

removeInconsistent (Set.empty.Add(consistentSet).Add(inconsistentSet))
    |> printf "The result of removeInconsistent (expected result [a, b, c]) is: %A \n"

toDNFsets (Dis (P "A", Neg (Con (P "B", Con (P "A", P "C"))))) 
    |> printfn "The result of toDNFsets (test 1) is: %A \n"

toDNFsets (Con (P "A", Neg (Con (P "B", Con (P "A", P "C")))))
    |> printfn "The result of toDNFsets (test 2) is: %A \n"

impl (atomA, atomB)
    |> printf "The result of a impl b is: %A \n"

impl (atomA, negA)
    |> printf "The result of a impl not a is: %A \n"

impl (negA, negA)
    |> printf "The result of not a impl not a is: %A \n"

iff (atomA, atomB)
    |> printf "The result of a iff a is: %A \n"

iff (atomA, negA)
    |> printf "The result of a iff not a is: %A \n"

iff (negA, negA)
    |> printf "The result of not a iff not a is: %A \n"

badProp (2, Con (Dis (P "A", P "B" ), Dis (P "A", P "B" )))
    |> printf "The result of badprop with n = 2 is: %A \n"

toDNFsets (badProp (2, Con (Dis (P "A", P "B" ), Dis (P "A", P "B" ))))
    |> printfn "The result of toDNFsets with badProp (n = 2) is: %A \n"