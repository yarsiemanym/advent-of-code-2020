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
                    for space in row do
                        yield parseSpace space
                ]
        ]
    }

let readFile =
    File.ReadAllLines
    >> parseSpaces

let determineDirectionsToCheck (x, y) (map:Map) =
    let xMin = if x <= 0 then 0 else x - 1
    let xMax = if x >= map.Width - 1 then map.Width - 1 else x + 1
    let yMin = if y <= 0 then 0 else y - 1
    let yMax = if y >= map.Height - 1 then map.Height - 1 else y + 1

    [
        for i in yMin .. yMax do
            for j in xMin .. xMax do
                if (j, i) <> (x, y) then
                    yield (j - x, i - y)
    ]

let countOccupiedAdjacentSeats (x, y) map =
    [
        for direction in (determineDirectionsToCheck (x, y) map) do
            yield map.GetCoord ((fst direction + x), (snd direction + y))
    ]
    |> List.filter (fun (space:Space) -> space.IsOccupied)
    |> List.length

let determineLineOfSight (x, y) (map:Map) (dx, dy) = 
    let xMagnitude = if dx < 0 then x / abs dx else if dx = 0 then Int32.MaxValue else (map.Width - 1 - x) / dx
    let yMagnitude = if dy < 0 then y / abs dy else if dy = 0 then Int32.MaxValue else (map.Height - 1 - y) / dy
    let magnitude = min xMagnitude yMagnitude

    [
        for i in 1 .. magnitude do
            yield (x + (i * dx), y + (i * dy))
    ]

let canSeeOccupiedSeat (x, y) map (dx, dy) =
   determineLineOfSight (x, y) map (dx, dy)
   |> List.map map.GetCoord
   |> List.filter (fun (space:Space) -> match space with | Seat(_) -> true | _ -> false)
   |> List.tryHead
   |> (fun seat -> if seat.IsNone then false else seat.Value.IsOccupied)

let countOccupiedVisibleSeats (x, y) map = 
    determineDirectionsToCheck (x, y) map
    |> List.filter (canSeeOccupiedSeat (x, y) map)
    |> List.length
    
let simulateOneRound occupiedSeatCounter threshold (map:Map)  =
    {
        Spaces =
        [
            for i in 0 .. map.Height - 1 do
                let row = map.Spaces.[i]
                [
                    for j in 0 .. map.Width - 1 do
                        let space = row.[j]

                        match space with
                        | Floor -> yield space
                        | Seat(occupied) -> 
                            if occupied && (occupiedSeatCounter (j, i) map) >= threshold then 
                                yield Seat(Occupied = false)
                            else if not occupied && (occupiedSeatCounter (j, i) map) = 0 then
                                yield Seat(Occupied = true)
                            else yield space
                ]
        ]
    }

let runSimulation occupiedSeatCounter threshold initialMap =
    let mutable currentMap = initialMap
    let mutable continueLoop = true

    while continueLoop do
        let newMap = simulateOneRound occupiedSeatCounter threshold currentMap
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

[<EntryPoint>]
let main argv =
    let originalMap = readFile argv.[0]
    
    originalMap
    |> runSimulation countOccupiedAdjacentSeats 4
    |> printfn "The answer to part 1 is '%d'."

    originalMap
    |> runSimulation countOccupiedVisibleSeats 5
    |> printfn "The answer to part 2 is '%d'."

    0