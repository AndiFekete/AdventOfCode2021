module day10

open common

type Stack = StackContent of List<char>

let push x stack =
    let (StackContent content) = stack
    StackContent (x::content)

let pop (StackContent stack) =
    match stack with
    |head::tail -> head, StackContent tail
    |[] -> ' ', StackContent []

let content (StackContent content) =
    content

let getError (line:List<char>) =
    let mutable stack = StackContent []
    let mutable result = 0
    let mutable i = 0
    while i < line.Length do
        let c = line.[i]
        if c='(' || c ='[' || c='{' || c='<' then
            stack <- push c stack
        else
            let top, tmpstack = pop stack
            stack <- tmpstack
            if c=')' && top <> '(' then
                result <- 3
            elif c=']' && top <> '[' then
                result <- 57
            elif c='}' && top <> '{' then
                result <- 1197
            elif c='>' && top <> '<' then
                result <- 25137
            else
                result <- 0 
        if result > 0 then
            i <- line.Length
        else
            i <- i+1
    result

let getClosingPair c =
    if c ='(' then
        ')'
    elif c='[' then
        ']'
    elif c='{' then
        '}'
    elif c='<' then
        '>'
    else
        failwith "Not an opening character"

let getPointvalue c =
    if c =')' then
        bigint 1
    elif c=']' then
        bigint 2
    elif c='}' then
        bigint 3
    elif c='>' then
        bigint 4
    else
        failwith "Not a closing character"

let getEndChars (line:List<char>) =
    let mutable stack = StackContent []
    let mutable result = []
    let mutable i = 0
    for c in line do
        if c='(' || c ='[' || c='{' || c='<' then
            stack <- push c stack
        else
            let top, tmpstack = pop stack
            stack <- tmpstack
            if top =' ' then
                result <- result @ [getClosingPair c]
    result @ ((content stack) |> List.map (fun c -> getClosingPair c))

let rec pointsFromEndChars (line:List<char>) =
    match line with
    |head::tail -> (getPointvalue head) + (bigint 5)*(pointsFromEndChars tail)
    |[] -> bigint 0

let part1 =
    let input = (readInput "..\..\..\inputs\day10.txt") |> List.map (fun s -> Seq.toList s)
    printfn "%A" (input |> List.map getError |> List.sum)

let part2 =
    let input = (readInput "..\..\..\inputs\day10.txt") |> List.map (fun s -> Seq.toList s)
    let endCharList = (input |> List.filter (fun line -> (getError line)=0) |> List.map (getEndChars))
    let points = (endCharList |> List.map (fun l -> l |> List.rev |> pointsFromEndChars) |> List.sort)
    printfn "%A" points
    printfn "%A" points.[points.Length/2]