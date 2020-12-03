open System
open System.IO

let readFile (filePath:string) = 
    [
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield Int32.Parse(sr.ReadLine ())
    ]

let sum (a:int, b:int) = a + b

let product (a:int, b:int) = a * b

let sumEquals2020 (a:int, b:int) = sum(a, b) = 2020

let buildTuples (entries:list<int>) = 
    [
        for i in 0 .. entries.Length - 1 do
            for j in 0 .. entries.Length - 1 do
                if i <> j then
                    yield (entries.Item(i), entries.Item(j))
    ]

let inspectTuples (tuples:list<int * int>) = 
    [
        for tuple in tuples do
            if sumEquals2020 tuple then
                yield product tuple
    ]
    
let findAnswers (entries:list<int>) = buildTuples(entries) |> inspectTuples

let findAnswer (entries:list<int>) =  findAnswers(entries).Head

let printAnswer (answer) =
    printfn "The answer is %d." answer
    0

[<EntryPoint>]
let main argv =
    readFile(argv.[0]) |> findAnswer |> printAnswer