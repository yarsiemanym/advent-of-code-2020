module AdventOfCode2020.Day01

open System.IO

let readFile =
    File.ReadAllLines 
    >> Array.map int 
    >> Array.toSeq

let sum (a, b, c) = a + b + c

let product (a, b, c) = a * b * c

let sumEquals2020 (a, b, c) = sum (a, b, c) = 2020

let buildTuples entries =
    seq {
        for i in 0 .. Seq.length (entries) - 1 do
            for j in 0 .. Seq.length (entries) - 1 do
                for k in 0 .. Seq.length (entries) - 1 do
                    if i <> j && j <> k && i <> k then 
                        yield (Seq.item i entries, Seq.item j entries, Seq.item k entries)
    }

let inspectTuples tuples =
    seq {
        for tuple in tuples do
            if sumEquals2020 (tuple) then 
                yield product (tuple)
    }

let printAnswer answer = printfn "The answer is '%d'." answer

let findAnswer =
    readFile
    >> buildTuples
    >> inspectTuples
    >> Seq.head
    >> printAnswer

[<EntryPoint>]
let main argv =
    findAnswer argv.[0]
    0
    