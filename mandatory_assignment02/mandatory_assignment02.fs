// ---------------------------------------------------------------
// Mandatory assignment 2
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

type CourseNo   = int
type Title      = string
type ECTS       = int
type CourseDesc = Title * ECTS 

type CourseBase = Map<CourseNo, CourseDesc>

type Mandatory   = Set<CourseNo>
type Optional    = Set<CourseNo> 
type CourseGroup = Mandatory * Optional

type BasicNaturalScience      = CourseGroup
type TechnologicalCore        = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective                 = CourseNo -> bool

type FlagModel  = BasicNaturalScience * TechnologicalCore 
                   * ProjectProfessionalSkill * Elective                 
type CoursePlan = Set<CourseNo>

let isValidCourseDesc = function
    | (title, ects) -> ects % 5 = 0 

let isValidCourseBase cb =
    Map.forall (fun key value -> isValidCourseDesc value) cb

let disjoint s1 s2 =
    Set.intersect s1 s2 = set[]

let sumECTS cs cb =
    Map.filter (fun key _ -> Set.contains key cs) cb
    |> Map.fold (fun acc _ (title, ects) -> acc + ects) 0

let isValidCourseGroup cg cb =
    let man = fst cg
    let opt = snd cg
    let sumMan = sumECTS man cb
    let sumOpt = sumECTS opt cb
    if disjoint man opt && sumMan = 45 && Set.count opt = 0 then true
    elif disjoint man opt && sumMan + sumOpt >= 45 && sumMan < 45 then true
    else false

let ep = (fun cn -> cn >= 20)

let disjointCrsGrps bns tc pps = 
    (disjoint (Set.union (fst bns) (snd bns)) (Set.union (fst tc) (snd tc))) && 
    (disjoint (Set.union (fst bns) (snd bns)) (Set.union (fst pps) (snd pps))) && 
    (disjoint (Set.union (fst tc) (snd tc)) (Set.union (fst pps) (snd pps)))

let isValid fm cb =
    let bns, tc, pps, ep = fm
    match isValidCourseGroup bns cb && isValidCourseGroup tc cb && isValidCourseGroup pps cb with
    | true -> 
        match disjointCrsGrps bns tc pps with
        | true -> 
            match (Set.forall ep (fst bns) && Set.forall ep (snd bns)
            && Set.forall ep (fst tc) && Set.forall ep (snd tc)
            && Set.forall ep (fst pps) && Set.forall ep (snd pps)) with
            | true -> true
            | _ -> false
        | _ -> false
    | _ -> false
            
let checkPlan cp fm cb =
    let bns, tc, pps, ep = fm
    match isValid fm cb with
    | true -> Set.forall ep cp && sumECTS cp cb = 180
    | false -> false

(* ---------------------------------------- Tests ----------------------------------------*)
let courseA = ("Test course", 12)
let courseB = ("Test course", 15)

isValidCourseDesc courseA 
    |> printf "The couseA is: %b \n"

isValidCourseDesc courseB 
    |> printf "The couseB is: %b \n"

let courseBaseA = Map.ofList [(1, courseA); (2, courseB)]
let courseBaseB = Map.ofList [(1, courseB); (2, courseB)]

isValidCourseBase courseBaseA
    |> printf "The couseBaseA is: %b \n"

isValidCourseBase courseBaseB
    |> printf "The couseBaseB is: %b \n"

let setA = set[1; 2; 3]
let setB = set[1; 4; 5]
let setC = set[6; 7; 8]

disjoint setA setB
    |> printf "Set A & B are: %b \n"

disjoint setA setC
    |> printf "Set A & C are: %b \n"

let setCoursesA = set[1; 2; 3]
let setCoursesB = set[1; 4; 5]
let setCoursesC = set[6; 7; 8]

sumECTS setCoursesA courseBaseA
    |> printf "The total ECTS points is: %i \n"

sumECTS setCoursesB courseBaseA
    |> printf "The total ECTS points is: %i \n"

let courseGroupA = (set[1; 2; 3], set[1; 2; 3])
let courseGroupB = (set[1; 4; 5], set[10; 2; 12])
let courseGroupC = (set[1; 4; 5], set[])
let courseGroupD = (set[1; 4], set[5; 6])

let courseC = ("Test course", 15)
let courseBaseC = Map.ofList [(1, courseC); (4, courseC); (5, courseC); (6, courseC)]

isValidCourseGroup courseGroupA courseBaseA
    |> printf "The course group is: %b \n"

isValidCourseGroup courseGroupB courseBaseC
    |> printf "The course group is: %b \n"

isValidCourseGroup courseGroupC courseBaseC
    |> printf "The course group is: %b \n"

isValidCourseGroup courseGroupD courseBaseC
    |> printf "The course group is: %b \n"