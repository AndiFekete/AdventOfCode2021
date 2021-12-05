module day4

open common

type BingoBoard = List<List<string>>
type BingoBoards = List<BingoBoard>


let parseInput (input:List<string>) =
    let numbers = input.[0].Split [|','|]
    let mutable boards = [] 
    let mutable temp = []

    for line in List.tail (List.tail input) do
        if line = "" then
            temp <- (List.rev temp)
            boards <- temp::boards
            temp <- []
        else
            temp <- List.filter (fun s-> s<>"") (Seq.toList (line.Split [|' '|])) :: temp
    temp <- (List.rev temp)
    boards <- temp::boards
    (numbers, (List.rev boards))

let updateNumber target original =
    if original=target then
        "X"
    else
        original

let updateRow (number:string) (row:List<string>) =
    row |> List.map (updateNumber number)

let updateBoard (number:string) (board:BingoBoard) =
    board |> List.map (updateRow number)
    
let rec transpose = function
    | (_::_)::_ as M -> 
        List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let checkRow (board:BingoBoard) =
    let mutable result = false
    for row in board do
        if List.length (List.filter (fun s -> s="X") row) = 5 then
            result <- true
        else
            result <- result || false
    result

let checkBoard (board:BingoBoard) =
    let hasRow = checkRow board
    let hasColumn = checkRow (transpose board)
    hasRow || hasColumn

let checkForBingo (boards:BingoBoards) =
    let results = boards |> List.map (checkBoard)
    if (List.length (List.filter (fun b -> b) results) > 0) then
        List.findIndex (fun b->b) results
    else
        -1

let checkForBingo2 prev cur n =
    if (List.length (List.filter (fun b->b) cur)) < n then
        -1
    else
        let zipped = List.zip prev cur
        let mutable result = -1
        try
            result <- List.findIndex (fun (a,b) -> a = false & b = true) zipped
        with
        |ex -> result <- -1

        result

let getEndResult (board:BingoBoard) (number:int) =
    let mutable sum = 0
    for row in board do
        for number in row do
            if number <> "X" then
                sum <- sum + int number
    number*sum

let part1 = 
    let input = readInput "..\..\..\inputs\day4.txt"
    let input = parseInput input

    let numbers = fst input
    let mutable boards = snd input
    let mutable i = 0

    while i<numbers.Length do
        boards <- boards |> List.map (updateBoard numbers.[i])
        let bingo = checkForBingo boards
        if bingo > -1 then
            let result = getEndResult boards.[bingo] (int numbers.[i])
            printfn "board %d : %A, result %d" bingo boards.[bingo] result
        
            i <- numbers.Length
        else
            i <- i + 1
    

let part2 =
    let input = readInput "..\..\..\inputs\day4.txt"
    let input = parseInput input

    let numbers = fst input
    let mutable boards = snd input
    let n = boards.Length
    let mutable i = 0

    while i<numbers.Length do
        let previousResults = boards |> List.map (checkBoard)
        boards <- boards |> List.map (updateBoard numbers.[i])
        let currentResults = boards |> List.map (checkBoard)

        let bingo = checkForBingo2 previousResults currentResults n
        if bingo > -1 then
            let result = getEndResult boards.[bingo] (int numbers.[i])
            printfn "board %d : %A, result %d" bingo boards.[bingo] result
        
            i <- numbers.Length
        else
            i <- i + 1