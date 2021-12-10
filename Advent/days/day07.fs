module day07

open common

let parseInput (input:string) =
    let list = Seq.toList (input.Split [|','|])
    list |> List.map (fun s -> int s)

let rec calculateFuel (crabs:List<int>) (goal:int) =
    match crabs with
    |head::tail -> abs (head-goal) + (calculateFuel tail goal)
    |[] -> 0

let kisGauss n =
    n*(n+1)/2

let rec calculateFuel2 (crabs:List<int>) (goal:int) =
    match crabs with
    |head::tail -> (kisGauss (abs (head-goal))) + (calculateFuel2 tail goal )
    |[] -> 0

let avg aList =        
    let sum, count = 
        List.fold (fun (sum, count) current -> (sum + current, count + 1)) (0,0) aList
    let average = sum / count
    average

let part1 =
    let input = readInput "..\..\..\inputs\day7.txt"
    let crabs = parseInput input.[0] |> List.sort
    let n = crabs.Length

    let fuel = calculateFuel crabs crabs.[n/2]
    printfn "%d" fuel

    if n%2 = 0 && crabs.[n/2-1] <> crabs.[n/2] then
        let fuel2 = calculateFuel crabs crabs.[n/2-1]
        if fuel > fuel2 then
           printf " %d" fuel2
        else
           printfn "%d" fuel
           
let part2 =
    let input = readInput "..\..\..\inputs\day7.txt"
    let crabs = parseInput input.[0] |> List.sort
    

    let mutable fuel = calculateFuel2 crabs crabs.[0]
    for i in crabs.[0]+1..crabs.[crabs.Length-1] do
        let tmp = calculateFuel2 crabs i
        if tmp < fuel then
            fuel <- tmp


    printfn "%A" fuel