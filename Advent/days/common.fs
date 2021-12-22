module common

open System.IO
open System

let readInput (fileName:string) =
    Seq.toList (File.ReadAllLines fileName)

let readCharMap (fileName:string) =
    let stringinput = readInput fileName
    stringinput |> List.map (fun line -> (Seq.toList line))
    
let readIntMap (fileName:string) =
    let stringinput = readInput fileName
    stringinput |> List.map (fun line -> (Seq.toList line |> List.map (fun c -> int c - int '0')))

let rec printmatrix (matrix:int[][]) =
    for line in matrix do
        printfn "%A" line

let addborder borderitem matrix= 
    let withsides = matrix |> Array.map (fun line -> Array.append (Array.append [|borderitem|] line) [|borderitem|])
    Array.append [|[| for i in 0..11 -> borderitem |]|]  (Array.append withsides [|[| for i in 0..11 -> borderitem|]|])

let toBin (hexChar:char) =
    match hexChar with
    |'0' -> [0;0;0;0]
    |'1' -> [0;0;0;1]
    |'2' -> [0;0;1;0]
    |'3' -> [0;0;1;1]
    |'4' -> [0;1;0;0]
    |'5' -> [0;1;0;1]
    |'6' -> [0;1;1;0]
    |'7' -> [0;1;1;1]
    |'8' -> [1;0;0;0]
    |'9' -> [1;0;0;1]
    |'A' -> [1;0;1;0]
    |'B' -> [1;0;1;1]
    |'C' -> [1;1;0;0]
    |'D' -> [1;1;0;1]
    |'E' -> [1;1;1;0]
    |'F' -> [1;1;1;1]
    |_ -> []

let rec binToInt bin : bigint =
    match bin with 
    |[1] -> 1I
    |[0] -> 0I
    |x::xs -> (bigint x) + ((binToInt xs ) * 2I)
    |[] -> 0I