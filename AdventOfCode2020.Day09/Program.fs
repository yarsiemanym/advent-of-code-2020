module AdventOfCode2020.Day09

open System
open System.IO

let readFile = 
    File.ReadAllLines
    >> Array.map (int64)
    >> Array.toList

let printAnswer answer = printfn "The answer is '%d'." answer

let isValid number (preamble:list<int64>) = 
    let mutable isValid = false
    for i in 0 .. preamble.Length - 1 do
        for j in 0 .. preamble.Length - 1 do
            if i <> j && preamble.[i] + preamble.[j] = number then
                isValid <- true
    isValid

let validateNumbers preambleLength (numbers:list<int64>) = 
    seq {
        for i in preambleLength .. numbers.Length - 1 do
            let number = numbers.Item i
            (number, isValid number numbers.[i-preambleLength-1 .. i-1])
    }


let findAnswer preambleLength = 
    readFile
    >> validateNumbers preambleLength
    >> Seq.filter (not << snd)
    >> Seq.map (fst)
    >> Seq.head
    >> printAnswer

[<EntryPoint>]
let main argv =
    findAnswer (int argv.[0]) argv.[1]
    0
