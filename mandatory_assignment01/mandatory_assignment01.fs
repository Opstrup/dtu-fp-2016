// ---------------------------------------------------------------
// Mandatory assignment 1
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

type phoneNumber        = int;;
type yearOfBirth        = int;;
type name               = string;;
type themesOfInterest   = string list;;
type clubMember         = (phoneNumber * yearOfBirth * themesOfInterest * name);;
type register           = clubMember list;;

// checkForMatchInList': 'a string * 'a string list -> bool
let rec checkForMatchInList' = function
    | (_, []) -> false
    | (itemA, listBHead::listBTail) when itemA = listBHead -> true
    | (itemA, listBHead::listBTail) when itemA <> listBHead -> checkForMatchInList' (itemA, listBTail) 
    | _ -> false

// filterListByAge': 'a int * register list * register list -> clubMember list 
let rec filterListByAge' = function
    | (_, [], resultList) -> resultList
    | (age, (no, yob, toi, na)::regList, resultList) when yob <= age -> filterListByAge'(age, regList, resultList@[(no, yob, toi, na)])
    | (age, (no, yob, toi, na)::regList, resultList) when yob > age -> filterListByAge'(age, regList, resultList)
    | _ -> [] 

// filterListByInterest': themesOfInterest * register list * register list -> clubMember list
let rec filterListByInterest' = function
    | ([], [], resultList) -> resultList
    | (i::t, (no, yob, toi, na)::regList, resultList) when checkForMatchInList' (i, toi) -> filterListByInterest'(i::t, regList, resultList@[(no, yob, toi, na)])
    | (i::t, (no, yob, toi, na)::regList, resultList) when not(checkForMatchInList' (i, toi)) -> filterListByInterest'(i::t, regList, resultList)
    | (_, _, resultList) -> resultList

// findMemberOfAge: 'a int * register list -> clubMember list
let findMemberOfAge age regList = filterListByAge' (age, regList, [])

// findMemberOfAge: themesOfInterest * register list -> clubMember list
let findMemberByInterest interest regList = filterListByInterest' (interest, regList, [])

(* ---------------------------------------- Tests ----------------------------------------*)

let firstClubMember : clubMember = (112, 1990, ["jazz"; "pool"], "John Doe")
let secondClubMember : clubMember = (113, 1991, ["tennis"; "cars"], "Jimmy Doe")
let thirdClubMember : clubMember = (114, 1992, ["music"; "jazz"], "Jill Doe")
let fourthClubMember : clubMember = (115, 1891, ["soccer"; "jazz"; "pool"], "Juli Doe")

let reg : register = [firstClubMember; secondClubMember; thirdClubMember; fourthClubMember]

checkForMatchInList' ("jazz", ["jazz"; "pool"; "tennis"]) |> printf "Is jazz in the list: %b \n\n" // True - Ends with first argument
checkForMatchInList' ("chess", ["jazz"; "pool"; "tennis"]) |> printf "Is chess in the list: %b \n\n" // False - Does not find chess

findMemberOfAge 1990 reg |> printf "Members with year of birth equal to or over 1990: %A \n\n" // Finds two members in the club, one with year of birth 1990 and one with 1891
findMemberByInterest ["jazz"] reg |> printf "Members who likes jazz: %A \n\n" // Finds three members in the club, who all likes jazz
findMemberByInterest ["jazz"] reg |> findMemberByInterest ["pool"] |> printf "Members who likes jazz and pool: %A \n\n" // Finds two members in the club, who likes jazz AND pool 
