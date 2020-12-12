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

    type Ferry = 
        {
            X:int
            Y:int
            Heading:char
        }

        member this.MoveNorth value =
            {
                X = this.X
                Y = this.Y - value
                Heading = this.Heading
            }

        member this.MoveEast value =
            {
                X = this.X + value
                Y = this.Y
                Heading = this.Heading
            }

        member this.MoveSouth value =
            {
                X = this.X
                Y = this.Y + value
                Heading = this.Heading
            }

        member this.MoveWest value =
            {
                X = this.X - value
                Y = this.Y
                Heading = this.Heading
            }

        member this.Turn value =
            let currentIndex = List.findIndex (fun d -> d = this.Heading) directions
            let newIndex = (directions.Length + currentIndex + (value / 90)) % directions.Length
            
            {
                X = this.X
                Y = this.Y
                Heading = directions.[newIndex]
            }

        member this.MoveForward value =
            {
                X = if this.Heading = 'E' then this.X + value else if this.Heading = 'W' then this.X - value else this.X
                Y = if this.Heading = 'S' then this.Y + value else if this.Heading = 'N' then this.Y - value else this.Y
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

    let manhattanDistance (x1, y1) (x2, y2) = (abs (x2 - x1)) + (abs (y2 - y1))

    [<EntryPoint>]
    let main argv =
        let instructions = readFile argv.[0]

        let start = { X=0; Y=0; Heading='E' }
        let finish1 = start.FollowInstructions instructions

        manhattanDistance (start.X, start.Y) (finish1.X, finish1.Y)
        |> printfn "The answer to part 1 is '%d'."

        0
