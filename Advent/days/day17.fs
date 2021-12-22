module day17

type TargetArea =
    {
        xLeft: int;
        xRight: int;
        yUpper: int;
        yLower: int;
    }

type Point = int*int

let drag x =
    if x>0 then
        x-1
    elif x<0 then
        x+1
    else
        x

let step (p:Point) (v:Point) =
    let p' = (fst p + fst v, snd p + snd v)
    let v' = (drag (fst v), (snd v)-1)
    (p',v')

let inTarget (x,y) (t:TargetArea) =
    match (x,y) with
    |(x,y) when x>=t.xLeft && x<=t.xRight && y>=t.yLower && y<=t.yUpper -> true
    |_ -> false

let willEndUpInTarget (t:TargetArea) (v:Point) =
    let mutable p' = (0,0)
    let mutable v' = v
    let mutable end' = false
    let mutable result = false

    while not end' do
        let p'',v'' = step p' v'
        p' <- p''
        v' <- v''
        if inTarget p' t then
            end'<- true
            result <- true
            printfn "%A" v
        if (fst p') > t.xRight || (snd p') < t.yLower then
            end' <- true
    result

let part1 = 
    List.sum [1..128] |> printfn "%A"

let part2 = 
    let target = { xLeft = 150; xRight = 171; yLower = -129; yUpper = -70;}
    let l = [for i in 0..300 -> i]
    let velocities = l |> List.map (fun i -> [for j in -150..150-> (i,j)]) |> List.concat
    
    let count = velocities |> List.filter (fun v -> willEndUpInTarget target v) |> List.length
    printfn "%A" count 
        

