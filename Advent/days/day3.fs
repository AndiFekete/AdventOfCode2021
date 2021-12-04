module day3

open System.IO

let binNot (c:char) =
    if c='1' then
        '0'
    else
        '1'

let rec reversedbinToInt s =
    match s with 
    |['1']|['0'] ->int s.[0] - int '0';
    |x::xs -> (int x - int '0') + (reversedbinToInt xs )* 2


let part1 = 
    let mutable lines = Seq.toList (File.ReadLines "..\..\..\inputs\day3.txt")
    let n = String.length lines.[0]
    let m = lines.Length
    let mutable gammaBin = ""

    for i in 0..n-1 do
        let ones =
            lines
            |> Seq.toList
            |> List.map(fun s -> s.[i])
            |> List.filter(fun c -> c='1')
        if ones.Length > m/2 then
            gammaBin <- gammaBin + "1"
        else 
            gammaBin <- gammaBin + "0"

    let gamma = 
       gammaBin
       |> Seq.toList
       |> List.rev
    let epsilon = 
        gamma
        |> List.map(binNot)
    
    let gammaValue = reversedbinToInt gamma
    let epsilonValue = reversedbinToInt epsilon
    let result = gammaValue*epsilonValue

    printfn "%d*%d = %d" gammaValue epsilonValue result

    
let part2 =
    let lines = Seq.toList (File.ReadLines "..\..\..\inputs\day3.txt")
    let n = String.length lines.[0]

    let mutable workinglines = lines;
    let mutable i = 0;
    let mutable oxigen = "";
    let mutable co2 = "";
    while i<n do
        let ones =
            workinglines
            |> Seq.toList
            |> List.map(fun s -> s.[i])
            |> List.filter(fun c -> c='1')
        let zeros =
            workinglines
            |> Seq.toList
            |> List.map(fun s -> s.[i])
            |> List.filter(fun c -> c='0')
        if ones.Length >= zeros.Length then
            workinglines <-
                workinglines
                |> List.filter(fun s -> s.[i] = '1')
        else 
            workinglines <-
                workinglines
                |> List.filter(fun s -> s.[i] = '0')

        if workinglines.Length = 1 then
            i <- n
            oxigen <- workinglines.[0]
        else
            i <- i+1

    i <- 0
    workinglines <- lines;
    while i<n do
        let ones =
            workinglines
            |> Seq.toList
            |> List.map(fun s -> s.[i])
            |> List.filter(fun c -> c='1')
        let zeros =
            workinglines
            |> Seq.toList
            |> List.map(fun s -> s.[i])
            |> List.filter(fun c -> c='0')
        if ones.Length < zeros.Length then
            workinglines <-
                workinglines
                |> List.filter(fun s -> s.[i] = '1')
        else 
            workinglines <-
                workinglines
                |> List.filter(fun s -> s.[i] = '0')
        
        if workinglines.Length = 1 then
            i <- n
            co2 <- workinglines.[0]
        else
            i <- i+1

    let oValue = 
        oxigen
        |> Seq.toList
        |> List.rev
        |> reversedbinToInt
    let co2Value = 
        co2
        |> Seq.toList
        |> List.rev
        |> reversedbinToInt
    let result = oValue * co2Value

    printfn "%d*%d = %d" oValue co2Value result