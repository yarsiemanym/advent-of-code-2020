module AdventOfCode2020.Day05

open System.IO
open System

let partition min max = ((max - min) / 2) + min

let rec calcLocation (key:string, min, max) =
    if key.Length = 0 || min >= max then
        max
    else
        match key.[0] with
        | 'F' | 'L' -> calcLocation (key.[1..], min, partition min max)
        | 'B' | 'R' -> calcLocation (key.[1..], partition min max, max)
        | _ -> raise (Exception (sprintf "Invalid parition key '%c' in '%s'" key.[0] key))

let locateSeat (key:string) =
    let rowKey = key.[0..6]
    let colKey = key.[7..9]
    (calcLocation (rowKey, 0, 127), calcLocation (colKey, 0, 7))

let locateSeats keys = 
    [
        for key in keys do
            locateSeat key
    ]

let calSeatId seatLocation = (fst(seatLocation) * 8) + snd(seatLocation)   

let calcSeatIds seatLocations =
    [
        for seatLocation in seatLocations do
            calSeatId seatLocation
    ]

let findUnoccupiedSeatId occupiedSeatIds =
    [List.min occupiedSeatIds .. List.max occupiedSeatIds]
    |> List.except occupiedSeatIds
    |> List.head
    
let printAnswer answer = printfn "The answer is '%d'." answer

let finadAnswer =
    File.ReadAllLines
    >> Array.toList
    >> locateSeats
    >> calcSeatIds
    >> findUnoccupiedSeatId
    >> printAnswer

[<EntryPoint>]
let main argv =
    finadAnswer argv.[0]
    0
