module day14

open common

let parseRule (line:string) =
    let parts = line.Split [|' '|]
    (parts.[0], parts.[2])


//if a pair appears multiple times in the template string, it still only returns the first occurance
//let getInsert (template:string) (rule:string*string) =
//    match template with
//    | s when s.Contains (fst rule) -> (s.IndexOf (fst rule) + 1, snd rule)
//    | _ -> (-1," ")

let rec getIndices (s:string) (ss:string) n =
    let ss' = Seq.toList ss
    match Seq.toList s with
    |a::b::rest when a = ss'.[0] && b = ss'.[1] -> (n+1)::(getIndices (new string [|for c in b::rest -> c|]) ss (n+1))
    |head::tail -> getIndices (new string [|for c in tail -> c|]) ss (n+1)
    |[] -> []

let getInserts (template:string) (rule:string*string) =
    match template with
    | s when s.Contains (fst rule) -> getIndices template (fst rule) 0 |> List.map (fun i -> (i,snd rule))
    | _ -> [(-1," ")]

let insert (s:string) (i,ss) =
    s.[0..i-1] + ss + s.[i..s.Length]

let rec insertChars (inserts:(int*string) list) (template:string) =
    match inserts with
    | head::tail -> insertChars tail (insert template head)
    | [] -> template

let part1 =
    let input = common.readInput "..\..\..\inputs\day14.txt"
    let mutable template = input.[0]

    let rules = input |> List.skip 2 |> List.map parseRule
    let mutable inserts = []

    for i in 0 .. 39 do
        printfn "%d" i
        let nyeh = rules |> List.map (fun r -> getInserts template r) 
        inserts <- nyeh |> List.concat |> List.filter (fun t -> fst t <> -1) |> List.sortBy (fun t -> - (fst t))
        template <- insertChars inserts template
    
    let charCounts = template |> Seq.toList |> List.countBy (fun c -> c) |> List.sortBy (fun t -> snd t)
    printfn "%A" charCounts
    let result = snd (charCounts.[charCounts.Length-1]) - snd (charCounts.Head)
    printfn "%d - %d = %d" (snd (charCounts.[charCounts.Length-1])) (snd (charCounts.Head)) result