module day5

open common

type Coordinates = int*int
type Line = Coordinates*Coordinates

let parseLine (line:string) =
    let pieces = line.Split [|' '|]
    let startCoord = pieces.[0].Split [|','|]
    let endCoord = pieces.[2].Split [|','|]

    (((int startCoord.[0]), (int startCoord.[1])),((int endCoord.[0]), (int endCoord.[1])))

let isVertical line=
    match line with
    |((x1,y1),(x2,y2)) when x1 = x2 -> true
    |_ -> false
    
let isHorizontal line =
    match line with
    |((x1,y1),(x2,y2)) when y1 = y2 -> true
    |_ -> false

let isDiagonal line =
    match line with
        |((x1,y1),(x2,y2)) when abs (x1-x2) = abs (y1-y2) -> true
        |_ -> false

let isPositive line =
    match line with
        |((x1,y1),(x2,y2)) when (x1-x2) = (y1-y2) -> true
        |_ -> false

//let createGrid coord =
//    List.init (fst coord) (fun index -> (List.init (snd coord) (fun index -> ".")))

let generateHorizontal (line:Line) =
    let mutable result = []
    let y = snd (fst line)
    let x1 = List.min [fst (fst line); fst (snd line)]
    let x2 = List.max [fst (fst line); fst (snd line)]

    for i in x1..x2 do
        result <- (i, y) :: result
    printfn "horizontal %A" result
    result

let generateVertical (line:Line) =
    let mutable result = []
    let x = fst (fst line)
    let y1 = List.min [snd (fst line); snd (snd line)]
    let y2 = List.max [snd (fst line); snd (snd line)]
        
    for i in y1..y2 do
        result <- (x, i) :: result
    printfn "vertical %A" result
    result

let generateDiagonal (line:Line) = 
    let mutable result = []
    if isPositive line then 
        let start = List.min [fst line; snd line]
        let finish = List.max [fst line; snd line]
        let steps = (fst finish) - (fst start)

        for i in 0..steps do
            result <- (fst start+i, snd start + i) :: result
    else
        let start = List.min [fst line; snd line]
        let finish = List.max [fst line; snd line]
        let steps = (fst finish) - (fst start)

        for i in 0..steps do
            result <- (fst start + i, snd start - i) :: result
    printfn "diagonal %A" result
    result

let getVents_part1 (line:Line) =
    match line with
    | line when isHorizontal line -> generateHorizontal line
    | line when isVertical line -> generateVertical line

let getVents_part2 (line:Line) =
    match line with
    | line when isHorizontal line -> generateHorizontal line
    | line when isVertical line -> generateVertical line
    | line when isDiagonal line -> generateDiagonal line

let part1 =
    let mutable input = (readInput "..\..\..\inputs\day5.txt") |> List.map parseLine
    input <- input |> List.filter (fun line -> isHorizontal line || isVertical line)

    let mutable vents = []
    for line in input do
        vents <- vents @ (getVents_part1 line)
    printfn "%A" (List.sort vents)

    let results = vents |> List.groupBy (fun foo -> foo) |> List.filter (fun t -> (snd t).Length > 1) |> List.map fst
    printfn "PART1: overlapping %A, count: %d" results results.Length
    printfn ""

let part2 =
    let mutable input = (readInput "..\..\..\inputs\day5.txt") |> List.map parseLine
    input <- input |> List.filter (fun line -> isHorizontal line || isVertical line || isDiagonal line)
    
    let mutable vents = []
    for line in input do
        vents <- vents @ (getVents_part2 line)
    printfn "%A" (List.sort vents)
    
    let results = vents |> List.groupBy (fun foo -> foo) |> List.filter (fun t -> (snd t).Length > 1) |> List.map fst
    printfn "PART 2: overlapping %A, count: %d" results results.Length  