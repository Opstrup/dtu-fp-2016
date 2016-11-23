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
    //| Dis (x, y) -> Set.union (litOf x) (litOf y)
    | _ -> Set.empty

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

// Part 8.
let rec badProp = function
    | (0, props) -> props
    | (n, props) -> badProp (n-1, Set.add (Con (P "A", P "C")) props) 

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
    |> printf "The result is %A \n"

propositionConverter notAandB
    |> printf "The result is %A \n"

propositionConverter notAorB
    |> printf "The result is %A \n"

literal CandAorB
    |> printf "The result is %A \n"

literal AorBandC
    |> printf "The result is %A \n"

litOf (AandB)
    |> printf "The result is %A \n"

litOf (Con (P "A", Neg (Con (P "B", Con (P "A", P "C"))))) 
    |> printfn "The result is %A \n"

dnfToSet (Dis (P "A", Neg (Con (P "B", Con (P "A", P "C"))))) 
    |> printfn "The result is %A \n"

isConsistent (Set.empty.Add(atomA).Add(atomB).Add(atomC))
    |> printf "The result is %A \n"

isConsistent (Set.empty.Add(atomA).Add(atomB).Add(atomC).Add(negA))
    |> printf "The result is %A \n"

removeInconsistent (Set.empty.Add(consistentSet).Add(inconsistentSet))
    |> printf "The result is %A \n"

toDNFsets (Dis (P "A", Neg (Con (P "B", Con (P "A", P "C"))))) 
    |> printfn "The result of toDNFsets is %A \n"

toDNFsets (Con (P "A", Neg (Con (P "B", Con (P "A", P "C")))))
    |> printfn "The result of toDNFsets is %A \n"

impl (atomA, atomB)
    |> printf "The result is %A \n"

impl (atomA, negA)
    |> printf "The result is %A \n"

impl (negA, negA)
    |> printf "The result is %A \n"

iff (atomA, atomB)
    |> printf "The result is %A \n"

iff (atomA, negA)
    |> printf "The result is %A \n"

iff (negA, negA)
    |> printf "The result is %A \n"

badProp (5, Set.empty)
    |> printf "The result of badprop is %A \n"