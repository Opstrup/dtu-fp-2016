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
type Description = (Outcome * string) list * float * string

let rec probOK = function
    | Branch(_, p, Leaf l1, Leaf l2) when p <= 1.0 && p >= 0.0      -> true
    | Branch(_, p, tl, tr) when p <= 1.0 && p >= 0.0                -> (probOK tl) && (probOK tr)
    | Branch(_, p, Leaf l1, tr) when p <= 1.0 && p >= 0.0           -> probOK tr
    | Branch(_, p, tl, Leaf l2) when p <= 1.0 && p >= 0.0           -> probOK tl
    | _                                                             -> false

// Outcome list * ProbTree -> bool
let rec isSample = function
    | ([], _) -> false
    | (os, Branch(ds, p, Leaf l1, tr)) when List.length os = 1 && os.Head = S -> true
    | (os, Branch(ds, p, tl, Leaf l2)) when List.length os = 1 && os.Head = F -> true
    | (h::tail, Branch(ds, p, tl, tr)) ->
        match h with
        | S -> isSample(tail, tl)
        | F -> isSample(tail, tr)
    | _ -> false

let rec descriptionOf' = function
    | (os, Branch(ds, p, tl, tr), (dsl, dp, s)) when List.length os > 1 && os.Head = F -> descriptionOf' (os.Tail, tr, (dsl @ [(os.Head, ds)], dp * (1.0 - p), ""))
    | (os, Branch(ds, p, tl, tr), (dsl, dp, s)) when List.length os > 1 && os.Head = S -> descriptionOf' (os.Tail, tl, (dsl @ [(os.Head, ds)], dp * (p), ""))
    | (os, Branch(ds, p, Leaf l1, Leaf l2), (dsl, dp, s)) when List.length os = 1 && os.Head = F -> (dsl @ [(os.Head, ds)], dp * (1.0 - p), l2)
    | (os, Branch(ds, p, Leaf l1, Leaf l2), (dsl, dp, s)) when List.length os = 1 && os.Head = S -> (dsl @ [(os.Head, ds)], dp * (p), l1)
    | _ -> failwith "Not a correct sample"
    
let descriptionOf os t = 
    if isSample(os, t) = false then failwith "Not a correct sample" else
        descriptionOf'(os, t, ([], 1.0, ""))

let rec allDescriptions = function
    | Branch(ds, p, Leaf ls, Leaf lf) ->
        set [([(S, ds)], 1.0-p, ls);([(F, ds)], p, lf)]
    | Branch(ds, p, ts, tf) -> 
        let descSetS = allDescriptions ts 
        let descSetF = allDescriptions tf
        let sSet = Set.map (fun (ls, ps, ln) -> ((S, ds)::ls, (1.0-p)*ps, ln)) descSetS
        let fSet = Set.map (fun (lf, pf, ln) -> ((F, ds)::lf, p*pf, ln)) descSetF
        Set.union sSet fSet
    | _ -> set []

let pred s = 
    s = "B" || s = "C"

let probabilityOf t =
    Set.fold (fun acc (_, p, s) -> if pred s then p + acc else acc ) 0.0 (allDescriptions t) 

(*-------- Test -------*)
let testTree = Branch(">2",0.67 , Branch(">3",0.5, Leaf "A", Leaf "B") 
                                , Branch(">3",0.5, Leaf "C", Leaf "D"))
let testTree2 = Branch(">2",0.67 , Branch(">3",0.5, Leaf "A", Leaf "B") 
                                , Branch(">3",2.5, Leaf "C", Leaf "D"))

probOK testTree
    |> printf "The result is %b : the result should be true \n"

probOK testTree2
    |> printf "The result is %b : the result should be false \n"

let outcomeTest = [F;S]
let outcomeTest2 = [S;F]
let outcomeTest3 = [S;F;S]

isSample(outcomeTest, testTree)
    |> printf "The result is %b : the result should be true \n"

isSample(outcomeTest2, testTree)
    |> printf "The result is %b : the result should be true \n"

isSample(outcomeTest3, testTree)
    |> printf "The result is %b : the result should be false \n"

descriptionOf outcomeTest testTree
    |> printf "The result is %A : the result should be ([(F, '>2'); (S, '>3')], 0.165, 'C')\n"

descriptionOf outcomeTest2 testTree
    |> printf "The result is %A : the result should be ([(F, '>2'); (S, '>3')], 0.335, 'B')\n"

allDescriptions testTree
    |> printf "The description of the whole tree is: %A\n"

pred "A"
    |> printf "The result of the predicet is %b : the result should be false \n"

pred "C"
    |> printf "The result of the predicet is %b : the result should be true \n"

pred "B"
    |> printf "The result of the predicet is %b : the result should be true \n"

probabilityOf testTree
    |> printf "Test of probability calculation on B and C: %f"