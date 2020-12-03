module AdventOfCode2020.Day01

open System
open System.IO

let readFile (filePath:string) = 
    [
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield Int32.Parse(sr.ReadLine ())
    ]

let sum (a:int, b:int, c:int) = a + b + c

let product (a:int, b:int, c:int) = a * b * c

let sumEquals2020 (a:int, b:int, c:int) = sum(a, b, c) = 2020

let buildTuples (entries:list<int>) = 
    [
        for i in 0 .. entries.Length - 1 do
            for j in 0 .. entries.Length - 1 do
                for k in 0 .. entries.Length - 1 do
                    if i <> j && j <> k  && i <> k then
                        yield (entries.Item(i), entries.Item(j), entries.Item(k))
    ]

let inspectTuples (tuples:list<int * int * int>) = 
    [
        for tuple in tuples do
            if sumEquals2020(tuple) then
                yield product(tuple)
    ]
    
let findAnswers (entries:list<int>) = buildTuples(entries) |> inspectTuples

let findAnswer (entries:list<int>) =  findAnswers(entries).Head

let printAnswer (answer) =
    printfn "The answer is %d." answer
    0

[<EntryPoint>]
let main argv =
    readFile(argv.[0]) |> findAnswer |> printAnswer