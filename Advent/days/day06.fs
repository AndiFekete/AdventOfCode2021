module day06

open common

let parseInput (input:string) =
    let list = Seq.toList (input.Split [|','|])
    list |> List.map (fun s -> int s)

//first try, threw StackOverFlowException for puzzle iput
let rec lanternfish list =
    match list with
    |x::xs when x = 0 -> 6 :: (lanternfish xs) @ [8]
    |x::xs -> x-1 :: (lanternfish xs)
    |[] -> []

//Modified first try to use tail recursion
//Part one was extremely slow
let rec lanternfish2 list result =
    match list with
    |x::xs when x = 0 -> lanternfish2 xs (6 :: result @[8])
    |x::xs -> lanternfish2 xs (x-1 :: result)
    |[] -> result

let update (count:List<bigint>) =
    let result = [count.[1]; //0
                  count.[2]; //1
                  count.[3]; //2
                  count.[4]; //3
                  count.[5]; //4
                  count.[6]; //5
                  count.[7] + count.[0]; //6
                  count.[8]; //8
                  count.[0];] //7
    result

let part1 =
    let input = common.readInput "..\..\..\inputs\day6.txt"
    let mutable list = parseInput input.[0]

    for i in 1..80 do
        list <- lanternfish2 list []
    printfn "%d" list.Length

let part2 =
    let input = common.readInput "..\..\..\inputs\day6.txt"
    let list = parseInput input.[0]
    let mutable fishCount = [bigint (list |> List.filter (fun i -> i = 0)).Length; 
                             bigint (list |> List.filter (fun i -> i = 1)).Length;
                             bigint (list |> List.filter (fun i -> i = 2)).Length;
                             bigint (list |> List.filter (fun i -> i = 3)).Length;
                             bigint (list |> List.filter (fun i -> i = 4)).Length;
                             bigint (list |> List.filter (fun i -> i = 5)).Length;
                             bigint (list |> List.filter (fun i -> i = 6)).Length;
                             bigint (list |> List.filter (fun i -> i = 7)).Length;
                             bigint (list |> List.filter (fun i -> i = 8)).Length]     //i'm sorry
    printfn "%A" fishCount
    for i in 1..256 do
        fishCount <- update fishCount
        
    printfn "%A" (List.sum fishCount) 