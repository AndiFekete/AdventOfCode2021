module day11

open common

let rec makeGlow (octopi:int[][]) sources =
    match sources with
    | (n,m)::tail -> let mutable s = tail 
                     for i in n-1..n+1 do
                        for j in m-1..m+1 do
                            if (octopi.[i].[j] > -1) then
                                octopi.[i].[j] <- octopi.[i].[j]+1
                                if (octopi.[i].[j] > 9) then 
                                    s <- s @ [(i,j)]
                                    octopi.[i].[j] <- -1
                     makeGlow octopi (s |> List.distinct) 
    | [] -> octopi

let rec setGlowToZero (octopi:int[][]) =
    for i in 1..10 do
        for j in 1..10 do
            if octopi.[i].[j] = -1 then
                octopi.[i].[j] <- 0
    octopi

let step (octopi:int[][]) =
    let mutable result = octopi
    let mutable glowup = []
    for i in 1..10 do
        for j in 1..10 do
            if octopi.[i].[j] = 9 then
                glowup <- glowup @ [(i,j)]
                octopi.[i].[j] <- -1
            else
                octopi.[i].[j] <- octopi.[i].[j]+1
    result <- makeGlow octopi glowup
    result |> setGlowToZero

let countZero (map:int[][]) =
    map |> Array.map (fun row -> (row |> Array.filter (fun i -> i = 0) |> Array.length)) |> Array.sum

let part1 =
    let mutable octopi = (readInput "..\..\..\inputs\day11.txt") |> List.map (fun line -> (Seq.toArray line) |> Array.map (fun c -> int c - int '0') )|> List.toArray |> common.addborder -1
    let mutable result = 0
    for i in 0..99 do
        octopi <- step octopi
        result <- result + countZero octopi

    common.printmatrix octopi
    printfn "%d" result

let rec getStepWhenAllZero (octopi:int[][]) n =
    match octopi with
    |_ when (countZero octopi) < 100 -> getStepWhenAllZero (step octopi) n+1
    |_ -> n

let part2 =
    let mutable octopi = (readInput "..\..\..\inputs\day11.txt") |> List.map (fun line -> (Seq.toArray line) |> Array.map (fun c -> int c - int '0') )|> List.toArray |> common.addborder -1
    let n = getStepWhenAllZero octopi 0
    printfn "%d" n