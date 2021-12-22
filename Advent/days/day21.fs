module day21

let part1 =
    let mutable p1 = 8
    let mutable p2 = 6

    let mutable p1score = 0
    let mutable p2score = 0

    let mutable die = 1
    let mutable rolls = 0
    let mutable next = 1

    while p1score < 1000 && p2score < 1000 do
        let steps = (die+1)*3
        rolls <- rolls + 3
        die <- (die + 3) % 100
        if next = 1 then
            p1 <- (p1 + (steps % 10)) % 10
            if p1 = 0 then p1 <-10
            p1score <- p1score + p1
            next <- 2
            printfn "p1 stepped to %d, score: %d" p1 p1score
        else
            p2 <- (p2 + (steps % 10)) % 10
            if p2 = 0 then p2 <-10
            p2score <- p2score + p2
            next <-1
            printfn "p2 stepped to %d, score: %d" p2 p2score
    
    let result = List.min [p1score; p2score] * rolls
    printfn "%A" result

type Player = int*int  //position, score
type Game = Player * Player
type Games = (Game * bigint) list

let probabilities = [(3,1I); (4,3I); (5,6I); (6,7I); (7,6I); (8,3I); (9,1I)]

let win (p:Player) =
    snd p >= 21

let roll (p:Player) (roll:int) =
    let mutable pos = (fst p + (roll % 10)) % 10
    if pos = 0 then pos <-10
    let score = snd p + pos
    Player (pos, score)

let rec updateGames1 (g:Games) =
    match g with
    |((p1,p2),n)::xs -> (probabilities |> List.map (fun p -> ((roll p1 (fst p), p2), n * snd p))) @ (updateGames1 xs)
    |[] -> []

let rec updateGames2 (g:Games) =
    match g with
    |((p1,p2),n)::xs -> (probabilities |> List.map (fun p -> ((p1, roll p2 (fst p)), n * snd p))) @ (updateGames2 xs)
    |[] -> []

let part2 =
    let p1 = Player (8,0)
    let p2 = Player (6,0)
    let mutable next = 1
    let mutable wins = [|0I;0I|]

    let mutable games = [((p1,p2), 1I)]
    while games.Length > 0 do
        if next = 1 then
            games <- updateGames1 games
            let won = games |> List.filter (fun g -> win (fst (fst g)))
            wins.[0] <- won |> List.map (fun g -> snd g) |> List.sum
            games <- games |> List.except won
            next <-2
        else
            games <- updateGames2 games 
            let won = games |> List.filter (fun g -> win (snd (fst g)))
            wins.[1] <- won |> List.map (fun g -> snd g) |> List.sum
            games <- games |> List.except won
            next <- 1