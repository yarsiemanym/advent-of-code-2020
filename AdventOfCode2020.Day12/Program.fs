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

    type Ferry1 =
        {
            Position:Point
            Heading:char
        }

        member this.MoveNorth value =
            {
                Position = 
                    { 
                        X = this.Position.X; 
                        Y = this.Position.Y + value
                    }
                Heading = this.Heading
            }

        member this.MoveEast value =
            {
                Position =
                    {
                        X = this.Position.X + value
                        Y = this.Position.Y
                    }
                Heading = this.Heading
            }

        member this.MoveSouth value =
            {
                Position = 
                    {
                        X = this.Position.X
                        Y = this.Position.Y - value
                    }
                Heading = this.Heading
            }

        member this.MoveWest value =
            {
                Position =
                    {
                        X = this.Position.X - value
                        Y = this.Position.Y
                    }
                Heading = this.Heading
            }

        member this.Turn value =
            let currentIndex = List.findIndex (fun d -> d = this.Heading) directions
            let newIndex = (directions.Length + currentIndex + (value / 90)) % directions.Length
            
            {
                Position = this.Position 
                Heading = directions.[newIndex]
            }

        member this.MoveForward value =
            {
                Position =
                    {
                        X = if this.Heading = 'E' then this.Position.X + value else if this.Heading = 'W' then this.Position.X - value else this.Position.X
                        Y = if this.Heading = 'S' then this.Position.Y - value else if this.Heading = 'N' then this.Position.Y + value else this.Position.Y
                    }
                Heading = this.Heading
            }

        member this.FollowInstruction instruction =
            match instruction.Operation with
            | 'N' -> this.MoveNorth instruction.Value
            | 'E' -> this.MoveEast instruction.Value
            | 'S' -> this.MoveSouth instruction.Value
            | 'W' -> this.MoveWest instruction.Value
            | 'L' -> this.Turn -instruction.Value
            | 'R' -> this.Turn instruction.Value
            | 'F' -> this.MoveForward instruction.Value
            | _ -> failwithf "Invalid instruction '%A'" instruction

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

        member this.MoveNorth value =
            {
                Position = this.Position
                WayPoint = 
                    { 
                        X = this.WayPoint.X; 
                        Y = this.WayPoint.Y + value
                    }
            }

        member this.MoveEast value =
            {
                Position = this.Position
                WayPoint = 
                    {
                        X = this.WayPoint.X + value
                        Y = this.WayPoint.Y
                    }
            }

        member this.MoveSouth value =
            {
                Position = this.Position
                WayPoint = 
                    {
                        X = this.WayPoint.X
                        Y = this.WayPoint.Y - value
                    }
            }

        member this.MoveWest value =
            {
                Position = this.Position
                WayPoint = 
                    {
                        X = this.WayPoint.X - value
                        Y = this.WayPoint.Y
                    }
            }

        member this.Turn value =
            let rotations = abs value / 90
            let direction = if value < 0 then -1 else if value = 0 then 0 else 1
            let mutable waypoint = this.WayPoint

            if direction = 0 then
                this
            else
                for r in 0 .. rotations - 1 do
                    waypoint <- 
                        {
                            X = waypoint.Y * direction
                            Y = waypoint.X * -1 * direction
                        }

                {
                    Position = this.Position
                    WayPoint = waypoint
                }

        member this.MoveForward value =
            {
                Position = 
                    {
                        X = this.Position.X + (value * this.WayPoint.X)
                        Y = this.Position.Y + (value * this.WayPoint.Y)
                    }
                WayPoint = this.WayPoint
            }

        member this.FollowInstruction instruction =
            match instruction.Operation with
            | 'N' -> this.MoveNorth instruction.Value
            | 'E' -> this.MoveEast instruction.Value
            | 'S' -> this.MoveSouth instruction.Value
            | 'W' -> this.MoveWest instruction.Value
            | 'L' -> this.Turn -instruction.Value
            | 'R' -> this.Turn instruction.Value
            | 'F' -> this.MoveForward instruction.Value
            | _ -> failwithf "Invalid instruction '%A'" instruction

        member this.FollowInstructions instructions =
            let mutable currentState = this

            for instruction in instructions do
                currentState <- currentState.FollowInstruction instruction
            
            currentState


    let parseInstruction (text:string) =
        {
            Operation = text.[0]
            Value = int text.[1..]
        }
    
    let parseInstructions lines =
        [
            for line in lines do
                yield parseInstruction line
        ]

    let readFile =
        File.ReadAllLines
        >> parseInstructions

    let manhattanDistance start finish = (abs (finish.X - start.X)) + (abs (finish.Y - start.Y))

    [<EntryPoint>]
    let main argv =
        let instructions = readFile argv.[0]

        let start1 = 
            { 
                Position =
                    {
                        X = 0
                        Y = 0
                    }
                Heading = 'E'
            }

        let finish1 = start1.FollowInstructions instructions
        manhattanDistance start1.Position finish1.Position
        |> printfn "The answer to part 1 is '%d'."

        let start2 = 
            { 
                Position =
                    {
                        X = 0
                        Y = 0
                    }
                WayPoint =
                    {
                        X = 10
                        Y = 1
                    }
            }

        let finish2 = start2.FollowInstructions instructions
        manhattanDistance start2.Position finish2.Position
        |> printfn "The answer to part 2 is '%d'."

        0
