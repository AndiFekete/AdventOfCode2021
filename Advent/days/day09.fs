module day09

open common

let first (a, _, _) = a
let second (_, b, _) = b
let third (_, _, c) = c

let getLowPoints (input:List<List<int>>) =
    let mutable lowPoints = []
    let n = input.Length
    let m = input.[0].Length
    for i in 0..n-1 do
        for j in 0..m-1 do
            let mutable isLowPoint = true
            let current = input.[i].[j]
            if (i>0) then
                isLowPoint <- isLowPoint && current<input.[i-1].[j]
            if (isLowPoint && i<n-1) then
                isLowPoint <- isLowPoint && current<input.[i+1].[j]
            if (isLowPoint && j>0) then
                isLowPoint <- isLowPoint && current<input.[i].[j-1]
            if (isLowPoint && j<m-1) then
                isLowPoint <- isLowPoint && current<input.[i].[j+1]

            if isLowPoint then lowPoints <- lowPoints @ [(i,j,current)]
    lowPoints

//ugly and throws StackOverflow on puzzle input
let rec getBasin (input:List<List<int>>) (lowPoint:int*int*int) =
    let n = input.Length
    let m = input.[0].Length
    let mutable basin = [(first lowPoint,second lowPoint)]
    match lowPoint with
    | (0,0,i) -> 
        if (input.[0].[1]>i && input.[0].[1]<>9) then basin <- basin @ (getBasin input (0,1,input.[0].[1]));
        if (input.[1].[0]>i && input.[1].[0]<>9) then basin <- basin @ (getBasin input (1,0,input.[1].[0]));
    | (x,y,i) when y = m-1 && x = n-1 -> 
            if (input.[x-1].[y]>i && input.[x-1].[y]<>9) then basin <- basin @ (getBasin input (x-1,y,input.[x-1].[y]));
            if (input.[x].[y-1]>i && input.[x].[y-1]<>9) then basin <- basin @ (getBasin input (x,y-1,input.[x].[y-1]));
    | (0,y,i) when y = m-1-> 
        if (input.[0].[y-1]>i && input.[0].[y-1]<>9) then basin <- basin @ (getBasin input (0,y-1,input.[0].[y-1]));
        if (input.[1].[y]>i && input.[1].[y]<>9) then basin <- basin @ (getBasin input (1,y,input.[1].[y]));
    | (x,0,i) when x = n-1->  
            if (input.[x-1].[0]>i && input.[x-1].[0]<>9) then basin <- basin @ (getBasin input (x-1,0,input.[x-1].[0]));
            if (input.[x].[1]>i && input.[x].[1]<>9) then basin <- basin @ (getBasin input (x,1,input.[x].[1]));    
    | (0,y,i) -> 
        if (input.[0].[y-1]>i && input.[0].[y-1]<>9) then basin <- basin @ (getBasin input (0,y-1,input.[0].[y-1]));
        if (input.[0].[y+1]>i && input.[0].[y+1]<>9) then basin <- basin @ (getBasin input (0,y+1,input.[0].[y+1]));
        if (input.[1].[y]>i && input.[1].[y]<>9) then basin <- basin @ (getBasin input (1,y,input.[1].[y]));
    | (x,0,i) ->  
            if (input.[x-1].[0]>i && input.[x-1].[0]<>9) then basin <- basin @ (getBasin input (x-1,0,input.[x-1].[0]));
            if (input.[x+1].[0]>i && input.[x+1].[0]<>9) then basin <- basin @ (getBasin input (x+1,0,input.[x+1].[0]));
            if (input.[x].[1]>i && input.[x].[1]<>9) then basin <- basin @ (getBasin input (x,1,input.[x].[1]));    
    | (x,y,i) when x = n-1 -> 
        if (input.[x].[y-1]>i && input.[x].[y-1]<>9) then basin <- basin @ (getBasin input (x,y-1,input.[x].[y-1]));
        if (input.[x].[y+1]>i && input.[x].[y+1]<>9) then basin <- basin @ (getBasin input (x,y+1,input.[x].[y+1]));
        if (input.[x-1].[y]>i && input.[x-1].[y]<>9) then basin <- basin @ (getBasin input (x-1,y,input.[x-1].[y]));
    | (x,y,i) when y = m-1 -> 
        if (input.[x-1].[y]>i && input.[x-1].[y]<>9) then basin <- basin @ (getBasin input (x-1,y,input.[x-1].[y-1]));
        if (input.[x+1].[y]>i && input.[x+1].[y]<>9) then basin <- basin @ (getBasin input (x+1,y,input.[x+1].[y]));
        if (input.[x].[y-1]>i && input.[x].[y-1]<>9) then basin <- basin @ (getBasin input (x,y-1,input.[x].[y-1]));
    | (x,y,i) -> 
        if (input.[x].[y-1]>i && input.[x].[y-1]<>9) then basin <- basin @ (getBasin input (x,y-1,input.[x].[y-1]));
        if (input.[x].[y+1]>i && input.[x].[y+1]<>9) then basin <- basin @ (getBasin input (x,y+1,input.[x].[y+1]));
        if (input.[x-1].[y]>i && input.[x-1].[y]<>9) then basin <- basin @ (getBasin input (x-1,y,input.[x-1].[y]));
        if (input.[x+1].[y]>i && input.[x+1].[y]<>9) then basin <- basin @ (getBasin input (x+1,y,input.[x+1].[y]));
    basin

let part1 =
    let stringinput = common.readInput "..\..\..\inputs\day9.txt"
    let input = stringinput |> List.map (fun line -> (Seq.toList line |> List.map (fun c -> int c - int '0')))
    let n = input.Length
    let m = input.[0].Length
    let result = (getLowPoints input) |> List.map (fun i -> (third i) + 1) |> List.sum
    printfn "%d" result

let part2 =
    let stringinput = common.readInput "..\..\..\inputs\day9.txt"
    let input = stringinput |> List.map (fun line -> (Seq.toList line |> List.map (fun c -> int c - int '0')))
    let lowPoints = getLowPoints input
    let basins = lowPoints |> List.map (fun p -> getBasin input p) |> List.map (fun l -> l |> List.distinct |> List.length) |> List.sort |> List.rev

    printfn "%A" basins
    let result = basins |> List.take 3 |> List.fold (fun a b -> a*b) 1
    printfn "%d" result
