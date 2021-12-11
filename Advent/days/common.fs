module common

open System.IO

let readInput (fileName:string) =
    Seq.toList (File.ReadAllLines fileName)

let readIntMap (fileName:string) =
    let stringinput = readInput fileName
    stringinput |> List.map (fun line -> (Seq.toList line |> List.map (fun c -> int c - int '0')))

let rec printmatrix (matrix:int[][]) =
    for line in matrix do
        printfn "%A" line
