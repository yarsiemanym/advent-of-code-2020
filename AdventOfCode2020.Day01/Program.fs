namespace AdventOfCode2020

open System.IO

module Day01 = 

    let readFile =
        File.ReadAllLines 
        >> Array.map int 
        >> Array.toList

    let sum numbers = List.fold (+) numbers

    let product numbers = List.fold (*) 1 numbers

    let sumEquals2020 numbers = List.sum numbers = 2020

    let buildTuples entries =
        seq {
            for i in 0 .. List.length (entries) - 1 do
                for j in 0 .. List.length (entries) - 1 do
                    if i <> j then 
                        yield [ List.item i entries; List.item j entries]
        }

    let buildTriples entries =
        seq {
            for i in 0 .. List.length (entries) - 1 do
                for j in 0 .. List.length (entries) - 1 do
                    for k in 0 .. List.length (entries) - 1 do
                        if i <> j && j <> k && i <> k then 
                            yield [ List.item i entries; List.item j entries; List.item k entries]
        }

    let inspect groups =
        seq {
            for group in groups do
                if sumEquals2020 (group) then 
                    yield product (group)
        }

    [<EntryPoint>]
    let main argv =
        let numbers = readFile argv.[0]

        buildTuples numbers
        |> inspect
        |> Seq.head
        |> printfn "The answer to part 1 is '%d'."

        buildTriples numbers
        |> inspect
        |> Seq.head
        |> printfn "The answer to part 2 is '%d'."

        0
        