namespace AdventOfCode2020

open System
open System.IO

module Day12 =

    let directions = [ 'N'; 'E'; 'S'; 'W' ]

    type Instruction =
        {
            Operation:char
            Value:int
        }

    type Point =
        {
            X:int
            Y:int
        }

        member this.ManhattanDistance point = (abs (point.X - this.X)) + (abs (point.Y - this.Y))

    type Ferry1 =
        {
            Position:Point
            Heading:char
        }

        member this.MoveNorth value = { this with Position = { this.Position with Y = this.Position.Y + value } }

        member this.MoveEast value = { this with Position = { this.Position with X = this.Position.X + value } }

        member this.MoveSouth value = { this with Position = { this.Position with Y = this.Position.Y - value } }

        member this.MoveWest value = { this with Position = { this.Position with X = this.Position.X - value } }


        member this.Turn value =
            let currentIndex = List.findIndex (fun d -> d = this.Heading) directions
            let newIndex = (directions.Length + currentIndex + (value / 90)) % directions.Length
            
            { this with Heading = directions.[newIndex] }

        member this.MoveForward value =
            let newPosition =
                {
                    X = match this.Heading with
                        | 'E' -> this.Position.X + value
                        | 'W' -> this.Position.X - value
                        | _ -> this.Position.X
          
                    Y = match this.Heading with
                        | 'N' -> this.Position.Y + value
                        | 'S' -> this.Position.Y - value
                        | _ -> this.Position.Y
                }

            { this with Position = newPosition }

        member this.FollowInstruction instruction =
            match instruction.Operation with
            | 'N' -> this.MoveNorth instruction.Value
            | 'E' -> this.MoveEast instruction.Value
            | 'S' -> this.MoveSouth instruction.Value
            | 'W' -> this.MoveWest instruction.Value
            | 'L' -> this.Turn -instruction.Value
            | 'R' -> this.Turn instruction.Value
            | 'F' -> this.MoveForward instruction.Value
            | _ -> failwithf "Invalid operation '%c'." instruction.Operation

        member this.FollowInstructions instructions =
            let mutable currentState = this

            for instruction in instructions do
                currentState <- currentState.FollowInstruction instruction
            
            currentState

    type Ferry2 = 
        {
            Position:Point
            WayPoint:Point
        }

        member this.MoveNorth value = { this with WayPoint = { this.WayPoint with Y = this.WayPoint.Y + value } }

        member this.MoveEast value = { this with WayPoint = { this.WayPoint with Y = this.WayPoint.Y } }

        member this.MoveSouth value = { this with WayPoint = { this.WayPoint with Y = this.WayPoint.Y - value } }

        member this.MoveWest value = { this with WayPoint = { this.WayPoint with Y = this.WayPoint.Y } }

        member this.Turn value =
            let rotations = abs value / 90
            let direction = if value < 0 then -1 else if value = 0 then 0 else 1
            let mutable waypoint = this.WayPoint

            if direction = 0 then
                this
            else
                for _ in 0 .. rotations - 1 do
                    waypoint <- 
                        {
                            X = waypoint.Y * direction
                            Y = waypoint.X * -1 * direction
                        }

                { this with WayPoint = waypoint }

        member this.MoveForward value = 
            let newPosition = 
                {
                    X = this.Position.X + (value * this.WayPoint.X)
                    Y = this.Position.Y + (value * this.WayPoint.Y)
                }

            { this with Position = newPosition }

        member this.FollowInstruction instruction =
            match instruction.Operation with
            | 'N' -> this.MoveNorth instruction.Value
            | 'E' -> this.MoveEast instruction.Value
            | 'S' -> this.MoveSouth instruction.Value
            | 'W' -> this.MoveWest instruction.Value
            | 'L' -> this.Turn -instruction.Value
            | 'R' -> this.Turn instruction.Value
            | 'F' -> this.MoveForward instruction.Value
            | _ -> failwithf "Invalid operation '%c'." instruction.Operation

        member this.FollowInstructions instructions =
            let mutable currentState = this

            for instruction in instructions do
                currentState <- currentState.FollowInstruction instruction
            
            currentState


    let parseInstruction (text:string) = { Operation = text.[0]; Value = int text.[1..] }
    
    let parseInstructions lines = [ for line in lines do yield parseInstruction line ]

    let readFile = File.ReadAllLines >> parseInstructions

    [<EntryPoint>]
    let main argv =
        let instructions = readFile argv.[0]

        let start1 = { Position = { X = 0; Y = 0 }; Heading = 'E' }
        let finish1 = start1.FollowInstructions instructions
        start1.Position.ManhattanDistance finish1.Position
        |> printfn "The answer to part 1 is '%d'."

        let start2 = { Position = { X = 0; Y = 0 }; WayPoint = { X = 10; Y = 1 } }
        let finish2 = start2.FollowInstructions instructions
        start2.Position.ManhattanDistance finish2.Position
        |> printfn "The answer to part 2 is '%d'."

        0
