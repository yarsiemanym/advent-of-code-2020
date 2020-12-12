namespace AdventOfCode2020.Day05

open System.IO

module Program = 

    let partition min max = ((max - min) / 2) + min

    let rec calcLocation (key:string, min, max) =
        if key.Length = 0 || min >= max then
            max
        else
            match key.[0] with
            | 'F' | 'L' -> calcLocation (key.[1..], min, partition min max)
            | 'B' | 'R' -> calcLocation (key.[1..], partition min max, max)
            | _ -> failwithf "Invalid parition key '%c' in '%s'" key.[0] key

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

    [<EntryPoint>]
    let main argv =
        let seatIds = 
            File.ReadAllLines argv.[0]
            |> Array.toList
            |> locateSeats
            |> calcSeatIds

        List.max seatIds
        |> printfn "The answer to part 1 is '%d'."

        findUnoccupiedSeatId seatIds
        |> printfn "The answer to part 2 is '%d'."

        0
