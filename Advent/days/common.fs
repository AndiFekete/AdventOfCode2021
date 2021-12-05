module common

open System.IO

let readInput (fileName:string) =
    Seq.toList (File.ReadAllLines fileName)