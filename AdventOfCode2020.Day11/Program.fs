module AdventOfCode2020.Day11

open System
open System.IO

type Space =
    | Floor
    | Seat of Occupied:bool

    member this.IsOccupied =
        match this with
        | Floor -> false
        | Seat(occupied) -> occupied

    member this.Symbol =
        match this with 
        | Floor -> '.'
        | Seat(occupied) -> if occupied then '#' else 'L' 

type Map =
    {
        Spaces:list<list<Space>>
    }

    member this.Height = List.length this.Spaces
    
    member this.Width = List.item 0 this.Spaces |> List.length
    
    member this.GetCoord (x, y) = List.item y this.Spaces |> List.item x
    
    member this.Print = 
        for row in this.Spaces do
            for space in row do
                printf "%c" space.Symbol
            printfn ""

    member this.GetVisibleDirections (x, y) = 
        let xMin = if x <= 0 then 0 else x - 1
        let xMax = if x >= this.Width - 1 then this.Width - 1 else x + 1
        let yMin = if y <= 0 then 0 else y - 1
        let yMax = if y >= this.Height - 1 then this.Height - 1 else y + 1

        [
            for i in yMin .. yMax do
                for j in xMin .. xMax do
                    if (j, i) <> (x, y) then
                        yield (j - x, i - y)
        ]

    member this.CountOccupiedAdjacentSeats (x, y) =
        [
            for direction in this.GetVisibleDirections (x, y) do
                yield this.GetCoord ((fst direction + x), (snd direction + y))
        ]
        |> List.filter (fun (space:Space) -> space.IsOccupied)
        |> List.length

    member this.DetermineLineOfSight (x, y) (dx, dy) =
        let xMagnitude = if dx < 0 then x / abs dx else if dx = 0 then Int32.MaxValue else (this.Width - 1 - x) / dx
        let yMagnitude = if dy < 0 then y / abs dy else if dy = 0 then Int32.MaxValue else (this.Height - 1 - y) / dy
        let magnitude = min xMagnitude yMagnitude

        [
            for i in 1 .. magnitude do
                yield (x + (i * dx), y + (i * dy))
        ]
     
    member this.CanSeeOccupiedSeat (x, y) (dx, dy) =
        this.DetermineLineOfSight (x, y) (dx, dy)
        |> List.map this.GetCoord
        |> List.filter (fun (space:Space) -> match space with | Seat(_) -> true | _ -> false)
        |> List.tryHead
        |> (fun seat -> if seat.IsNone then false else seat.Value.IsOccupied)

    member this.CountOccupiedVisibleSeats (x, y) =
        this.GetVisibleDirections (x, y)
        |> List.filter (this.CanSeeOccupiedSeat (x, y))
        |> List.length

    member this.SimulateOneRound occupiedSeatCounter threshold =
        {
            Spaces =
            [
                for i in 0 .. this.Height - 1 do
                    let row = this.Spaces.[i]
                    [
                        for j in 0 .. this.Width - 1 do
                            let space = row.[j]

                            match space with
                            | Floor -> yield space
                            | Seat(occupied) -> 
                                if occupied && (occupiedSeatCounter this (j, i)) >= threshold then 
                                    yield Seat(Occupied = false)
                                else if not occupied && (occupiedSeatCounter this (j, i)) = 0 then
                                    yield Seat(Occupied = true)
                                else yield space
                    ]
            ]
        }

    member this.RunSimulation occupiedSeatCounter threshold =
        let mutable currentMap = this
        let mutable continueLoop = true

        while continueLoop do
            let newMap = currentMap.SimulateOneRound occupiedSeatCounter threshold
            continueLoop <- newMap <> currentMap
            currentMap <- newMap

        currentMap.Spaces
        |> List.concat
        |> List.choose (fun space -> 
            match space with
            | Seat(occupied) -> Some occupied
            | _ -> None) 
        |> List.filter ((=) true)
        |> List.length


let parseSpace symbol = 
    match symbol with
    | '.' -> Floor
    | 'L' -> Seat(Occupied = false)
    | '#' -> Seat(Occupied = true)
    | _ -> failwithf "Invalid space '%c'" symbol

let parseSpaces rows =
    {
        Spaces = 
        [
            for row in rows do
                [
                    for symbol in row do
                        yield parseSpace symbol
                ]
        ]
    }

let readFile =
    File.ReadAllLines
    >> parseSpaces

let countOccupiedAdjacentSeats (map:Map) = map.CountOccupiedAdjacentSeats

let countOccupiedVisibleSeats (map:Map) = map.CountOccupiedVisibleSeats

[<EntryPoint>]
let main argv =
    let map = readFile argv.[0]
    
    map.RunSimulation countOccupiedAdjacentSeats 4
    |> printfn "The answer to part 1 is '%d'."

    map.RunSimulation countOccupiedVisibleSeats 5
    |> printfn "The answer to part 2 is '%d'."

    0