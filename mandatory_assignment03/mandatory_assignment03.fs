// ---------------------------------------------------------------
// Mandatory assignment 3
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

type Outcome = | S | F // S: for success and F: for failure
type Sample = Outcome list
type ProbTree = | Branch of string * float * ProbTree * ProbTree | Leaf of string
    
let rec probOK = function
    | Branch(_, p, Leaf l1, Leaf l2) when p <= 1.0 && p >= 0.0      -> true
    | Branch(_, p, b1, b2) when p <= 1.0 && p >= 0.0                -> (probOK b1) && (probOK b2)
    | Branch(_, p, Leaf l1, b2) when p <= 1.0 && p >= 0.0           -> probOK b2
    | Branch(_, p, b1, Leaf l2) when p <= 1.0 && p >= 0.0           -> probOK b1
    | _                                                             -> false

let rec isSample = function
    | ([], _) -> false
    | (os, t) when List.length os = 1 -> true
    | (h::t, (_, p, b1, b2)) ->
        match h with
        | S -> isSample t b1
        | F -> isSample t b2 


(*-------- Test -------*)
let testTree = Branch(">2",0.67 , Branch(">3",0.5, Leaf "A", Leaf "B") 
                                , Branch(">3",0.5, Leaf "C", Leaf "D"))

let testTree2 = Branch(">2",0.67 , Branch(">3",0.5, Leaf "A", Leaf "B") 
                                , Branch(">3",2.5, Leaf "C", Leaf "D"))

probOK testTree
    |> printf "The result is %b : the result should be true \n"

probOK testTree2
    |> printf "The result is %b : the result should be false \n"