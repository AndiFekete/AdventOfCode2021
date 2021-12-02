module day1
open System.IO

let readLines (filePath:string) = [|
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        let s = sr.ReadLine ()
        yield int s
|]

let day1 = 
    let input = readLines "input.txt"
    let intInput = 
        input
        |> Array.map(fun x -> int x)

    let mutable count = 0
    for i in 1.. intInput.Length-1 do
        if intInput.[i]>intInput.[i-1] then
            count <- count + 1

    printfn "%i" count

    count <-0
    let windows = [|
        for i in 0..intInput.Length-3 do
            yield Array.sum intInput.[i..i+2]
    |]

    for i in 1.. windows.Length-1 do
        if windows.[i]>windows.[i-1] then
            count <- count + 1

    printfn "%d" count