module day18

open common
open System

//will be treating numbers as list of characters (in strings)
type SnailfishNumber = string list

//homemade type check
let isInt (s:string) =
    try
        int s |> ignore
        true
    with
        | ex -> false
 
let parseLine (s:string) =
    let chars = Seq.toList s
    chars |> List.map (fun c -> c.ToString()) |> List.filter (fun s -> s<>",")

let print (n:SnailfishNumber) =
    printfn "%A" (n |> List.fold (+) "") 

let add (a:SnailfishNumber) (b:SnailfishNumber) =
    "[" :: a @ b @ ["]"]

//create the string list to insert instead of the original number
let splitNumber (n:int)=
    if n%2 = 0 then
        ["[";string (n/2);string (n/2);"]"]
    else
        ["[";string (n/2);string (n/2+1);"]"]

    
let rec split (n:SnailfishNumber) =
    match n with
    | x::xs when x = "[" || x = "]" -> x :: (split xs)
    | x::xs when (int x) > 9 -> splitNumber (int x) @ xs    //if one splittable number was found, return the rest as is
    | x::xs -> x :: (split xs)
    |[] -> [""]

let split2 (n:SnailfishNumber) =
    let i = n |> List.findIndex (fun s -> (isInt s) && (int s > 9))
    n.[0..i-1] @ splitNumber (int n.[i]) @ n.[i+1..n.Length-1]

let canSplit (n:SnailfishNumber) =
    (n |> List.filter (fun s -> isInt s) |> List.filter (fun s -> int s > 9) |> List.length) > 0

let rec canExplode (n:SnailfishNumber) (d:int) =
    if d > 4 then
        n.Length    //return "position" of 4-times nested pair
    else
        match n with
        |"["::xs -> canExplode xs (d+1)
        |"]"::xs -> canExplode xs (d-1)
        |x::xs -> canExplode xs d
        |[] -> -1   //no pairs nested 4 times

//________[x,y]___________
//        ^
//   pos length-d-1

let rec explodeRight l value =
    match l with
    | x::xs when isInt x -> string (value+(int x)) :: xs
    | x::xs -> x :: (explodeRight xs value)
    | [] -> []

let explodeLeft l value =
    List.rev (explodeRight (List.rev l) value)

let explode (n:SnailfishNumber) (d:int) =
    let startIndex = n.Length - d - 1     
    let rightValue = int n.[startIndex+2]
    let leftValue = int n.[startIndex+1]

    let mutable rightSide = n.[startIndex+4..n.Length-1]
    let mutable leftSide = n.[0..startIndex-1]
    
    rightSide <- explodeRight rightSide rightValue
    leftSide <- explodeLeft leftSide leftValue
    
    leftSide @ ["0"] @ rightSide

let reduce (n:SnailfishNumber) =
    let d = canExplode n 0
    if (d > -1) then
        printfn "explode"
        explode n d
    elif canSplit n then
        printfn "split"
        split2 n
    else
        n

let magnitude (n:SnailfishNumber) =
    let mutable result = n |> List.toArray
    while result.Length > 1 do 
        for i in 0..result.Length-1 do
            if isInt result.[i] then
                if isInt result.[i-1] then
                    //found right
                    result.[i] <- string ((int result.[i-1]*3)+((int result.[i])*2))
                    //delete parentheses and i-1th item
                    result.[i-2] <- "del"
                    result.[i-1] <- "del"
                    result.[i+1] <- "del"
        result <- result |> Array.filter (fun s -> s<> "del")
        print (result |> Array.toList)
    //lastly, only the magnitude should remain
    int result.[0]

//let part1 =
//    let input = common.readInput "..\..\..\inputs\day18.txt" |> List.map parseLine
//    let mutable sum = input.Head

//    for number in input.Tail do
//        sum <- add sum number
//        printfn "start sum"
//        print sum
//        let mutable tmp = reduce sum

//        while sum <> tmp do
//            sum<-tmp
//            print sum
//            tmp <- reduce sum

//        printfn "reduced sum"
//        print sum

//    printfn "%A" (magnitude sum)

let part2 =
    let input = common.readInput "..\..\..\inputs\day18.txt" |> List.map parseLine
    let mutable maxMagnitude = 0
    let mutable i = 0

    let pairs = input |> List.allPairs input |> List.filter (fun (a,b) -> a<>b)
    for (a,b) in pairs do
        i<- i+1
        printfn "%Ath pair" i
        let mutable sum = add a b
        let mutable tmp = reduce sum
        
        while sum <> tmp do
            sum<-tmp
            tmp <- reduce sum

        let m = magnitude sum
        if m>maxMagnitude then
            maxMagnitude <- m
            printfn "new max %A+%A %A" a b m
