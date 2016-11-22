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
    | P _ as x -> x

// Part 3.
let literal = function
    | Con (x, Dis (y, z)) -> Dis(Con(x, y), Con(x, z))
    | Con (Dis (y, z), x) -> Dis(Con(x, y), Con(x, z))
    | x -> x

// Part 4.

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