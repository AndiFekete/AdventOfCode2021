module day15

open common
open System

let createQueue n m =
   let mutable q = []
   for i in 0..n-1 do
        for j in 0..m-1 do
            q <- q @ [(i,j)]
   q

let createDist n m =
   let mutable d = []
   for i in 0..n-1 do
        for j in 0..m-1 do
            d <- (Int32.MaxValue, (i,j)) :: d
   (0,(0,0)) :: (d |> List.except [(Int32.MaxValue, (0,0))] )

let neighbours (i,j) n m =
    let mutable result = []
    if i>0 then
        result <- (i-1,j) :: result
    if j>0 then
        result <- (i,j-1) :: result
    if j<m-1 then
        result <- (i,j+1) :: result
    if i<n-1 then
        result <- (i+1,j) :: result

    result

let part1 =
    let map = common.readIntMap "..\..\..\inputs\day15.txt"
    let n = map.Length
    let m = map.[0].Length
    //map can be a graph
    //edges from i,j are
    //i,j+1, i,j-1, i+1,j, i-1,j with cost of risk level of end node
    //least weight path from 0,0 to n-1,n-1

    let mutable dist = createDist n m //cost * (i*j)
    let mutable prev = []
    let mutable queue = createQueue n m
    let mutable u = (-1,-1)
    
    while queue.Length > 0 do
        u <- dist |> List.filter (fun t -> List.contains (snd t) queue)|> List.minBy fst |> snd
        let distu = dist |> List.find (fun t -> snd t = u) |> fst
        queue <- queue |> List.except ([u])
        let neighbours = neighbours u n m
        for v in (neighbours) do
            if (queue |> List.contains v) then
                let distv = dist |> List.find (fun t -> snd t = v) |> fst
                let c = distu + map.[fst v].[snd v]
                if c < distv then
                    dist <- (c,v) :: (dist |> List.except [(distu, v)])
                    prev <- (v,u) :: prev
       

    printfn "%A" (dist |> List.find (fun t -> snd t = (n-1,m-1)))