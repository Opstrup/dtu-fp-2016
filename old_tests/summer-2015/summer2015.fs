// Summer 2015

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
         


(* ---- Test ---- *)
printf "Exercise01\n"
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
        