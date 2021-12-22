module day13

open common

let stringToPoint (s:string) =
    let split = s.Split [|','|]
    (int split.[0], int split.[1])

let rec parsePoints (input: List<string>) =
    match input with
    |""::xs -> 
            parsePoints xs
    |x::xs when x.StartsWith("fold") -> 
            parsePoints xs
    |x::xs -> 
            (stringToPoint x) :: (parsePoints xs)
    |[] -> []
            
let parseFold (s:string) =
    let fold = (s.Split [|' '|]).[2]
    let split = fold.Split [|'='|]
    (split.[0], int split.[1])

let rec foldUp (by:int) (dots:(int*int) list) =
    match dots with
    |(x,y)::xs when y>by -> (x, 2*by - y) :: foldUp by xs
    |x::xs ->  x :: foldUp by xs
    |[] -> []

let rec foldLeft (by:int) (dots:(int*int) list) =
    match dots with
    |(x,y)::xs when x>by -> (2*by - x, y) :: foldLeft by xs
    |x::xs -> x :: foldLeft by xs
    |[] -> []

let fold (f:string*int) dots =
    if (fst f)="y" then
        foldUp (snd f) dots
    else
        foldLeft (snd f) dots

let rec contains elem list =
    match list with
    |x::xs when x=elem -> true
    |x::xs -> contains elem xs
    |[] -> false

let printPaper dots =
    let n = dots |> List.map (fun t -> fst t) |> List.max
    let m = dots |> List.map (fun t -> snd t) |> List.max

    for i in 0..m do
        for j in 0..n do
            if contains (j,i) dots then
                printf "#"
            else
                printf "."
        printfn ""

let part1 =
    let input = common.readInput "..\..\..\inputs\day12.txt"
    let mutable dots = input |> parsePoints
    let folds = input |> List.filter (fun s -> s.StartsWith("fold")) |> List.map parseFold

    let dots2 = (fold folds.[0] dots) |> List.distinct
    printfn "%A" dots2.Length

let part2 =
    let input = common.readInput "..\..\..\inputs\day12.txt"
    let mutable dots = input |> parsePoints
    let folds = input |> List.filter (fun s -> s.StartsWith("fold")) |> List.map parseFold
    
    for f in folds do
        dots <- fold f dots

    printPaper dots
