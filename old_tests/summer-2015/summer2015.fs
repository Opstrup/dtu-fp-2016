// Summer 2015

// Problem 1
let repeat (s: string) (n: int) =
    if n = 0 then "" else
    String.replicate n s

// Tail recursive variant of repeat
let rec repeat2 s n =
    if n <= 0 then
        ""
    else
        s + (repeat s (n-1))

let f (s1: string) (s2: string) (n: int) =
    String.replicate (n/2) (s1 + "\n" + s2 + "\n")

let viz (m: int) (n: int) =
    f (repeat "XO" m) (repeat "OX" m) n

// Problem 2
let rec mixMap = function
    | ([], l2, acc) -> acc
    | (l1, [], acc) -> acc
    | (h1::l1, h2::l2, acc) ->  mixMap (l1, l2, acc @ [(h1, h2)]) 

let mixMapZip x y = 
    Seq.zip x y |> List.ofSeq

let rec unmixMap = function
    | ([], acc) -> acc
    | ((x, y) :: l1, (acc1, acc2)) -> unmixMap (l1, (acc1 @ [x], acc2 @ [y]))

let unmixMapUnzip l =
    List.unzip l

(* ---- Test ---- *)
printf "Problem01\n"
printf "Part 1\n"
repeat "ab" 4 
    |> printf "%s"

printf "\n"
printf "Part 2\n"
f "ab" "cd" 4
    |> printf "%s"

printf "\n"
printf "Part 3\n"
viz 4 5
    |> printf "%s"

printf "\n"
printf "Problem02\n"
printf "Part 1 - rec\n"
mixMap ([1; 2; 3; 4], [5; 6; 7; 8], [])
    |> printf "%A"

printf "\n"
printf "Part 1 - zip\n"
mixMap ([1; 2; 3; 4], [5; 6; 7; 8], [])
    |> printf "%A"

printf "\n"
printf "Part 2 - unzip\n"
unmixMap ([(1, 2);(3, 4);(5, 6)], ([],[]))
    |> printf "%A"