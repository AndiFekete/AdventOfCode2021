module day20

open common

let addborder matrix =
    common.addborder '.' matrix

//let pixelCode window =
//    let code = window |> List.concat |> List.map (fun c -> (int c)%2)       //int # = 35, int . = 46

//let part1 =
//    let input = common.readCharMap "..\..\..\inputs\day20.txt"
