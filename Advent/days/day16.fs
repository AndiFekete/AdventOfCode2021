module day16

open common
open System

type Packet =
    | Literal of (bigint*bigint)*bigint
    | Operator of (bigint*bigint)*(Packet list)

let rec versionSum (p:Packet) =
    match p with
    | Literal ((version, op), value) -> version
    | Operator ((version, op), packets) -> version + (packets |> List.map versionSum |> List.sum)

let greater a b =
    if a > b then
         1I
    else 0I

let less a b = 
    if a < b then
        1I
    else 0I

let equal a b = 
    if a = b then
        1I
    else 0I

let rec packetValue (p:Packet) =
    match p with
    | Literal (_,value) -> value
    | Operator ((_,op), packets) -> match op with
                                    |o when o = 0I -> packets |> List.map packetValue |> List.sum
                                    |o when o = 1I -> packets |> List.map packetValue |> Seq.reduce (*)
                                    |o when o = 2I -> packets |> List.map packetValue |> List.min
                                    |o when o = 3I -> packets |> List.map packetValue |> List.max
                                    |o when o = 5I -> let p = (packets |> List.map packetValue); 
                                                      greater p.[0] p.[1];
                                    |o when o = 6I -> let p = (packets |> List.map packetValue); 
                                                      less p.[0] p.[1];
                                    |o when o = 7I -> let p = (packets |> List.map packetValue); 
                                                      equal p.[0] p.[1];
                                    |_ -> 0I
let toString (input:int list) =
    List.fold (fun s i -> s + string i) "" input

let rec parseByLength (packet:int list) =
    let length = int (binToInt (List.rev (packet.[0..14])))
    printfn "length %d" length
    let mutable packets = []
    let mutable rest' = [] 
    let packet' = packet.[15..15+length-1]
    let rem = packet.[15+length..packet.Length-1]   //this is the remainder of the packet after parsing length long part
    let parsed,rest = parsePacket packet'
    packets <- [parsed]
    rest' <- rest
    while rest'.Length>0 do
        let parsed,rest = parsePacket rest'
        packets <- packets@[parsed]
        rest' <- rest
    packets,rem
    
and parseByNumber (packet:int list) =
    let number = int (binToInt (List.rev packet.[0..10]))
    printfn "number of packets: %d" number
    let mutable working = packet.[11..packet.Length-1]
    let mutable packets = []    
    while packets.Length<number do
        printfn "parsed: %d" packets.Length
        let (parsed,rest) = parsePacket working
        packets <- packets @ [parsed]
        working <- rest
    (packets,working)

and parseOperator (packet:int list) =
    if packet.[0] = 0 then
        printfn "parseByLength"
        parseByLength packet.Tail   //list of packets, rest of data
    else 
        printfn "parseByNumber"
        parseByNumber packet.Tail   //list of packets, rest of data

and parseLiteral(packet:int list) (content:int list) : bigint*(int list) =
    let fives = packet |> List.take 5
    match fives with
    |0::rest -> ((content@rest) |> List.rev |> binToInt, packet.[5..packet.Length-1]) //contents as int, rest of the packet data
    |1::rest -> parseLiteral packet.[5..packet.Length] (content@rest)
    |_ -> (0I,[])

and parsePacket (packet:int list) : Packet*(int list) =
    let version = packet |> List.take 3 |> List.rev |> binToInt
    match packet.[3..5] with
    |[1;0;0] -> let p = parseLiteral packet.[6..packet.Length-1] []
                (Literal ((version, 4I), fst p ), snd p ); //literal, rest of packet data
    |op  -> let p = parseOperator packet.[6..packet.Length-1];
            let op' = binToInt (List.rev op);
            (Operator ((version, op'), fst p ), snd  p ) //operator, rest of packet data


let part1and2 =
    let input = (common.readInput "..\..\..\inputs\day16.txt") |> List.head |> Seq.toList |> List.map common.toBin |> List.concat
    let p = parsePacket input 

    printfn "%A" p
    printfn "version sum: %A" (versionSum (fst p))
    printfn "packet value: %A" (packetValue (fst p))