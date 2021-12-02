// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let readLines (filePath:string) = [|
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        let s = sr.ReadLine ()
        yield int s
|]

let parseLine (input:string) =
    let split = input.Split [|' '|]
    let direction, dist = split.[0], int split.[1]
    (direction, dist) 

[<EntryPoint>]
let main argv =

    let lines = IO.File.ReadLines "input.txt"
    let course = 
        lines
       |> Seq.map parseLine 

    let mutable distance = 0
    let mutable depth = 0
    let mutable aim = 0

    for step in course do
        let d = snd step
        if (fst step).Equals("forward") then
            distance <- distance + d
            depth <- depth + aim*d
        elif (fst step).Equals("up") then
            aim <- aim - d
        else
            aim <- aim + d

    let result = depth*distance
    printf "%d" result

    0 // return an integer exit code