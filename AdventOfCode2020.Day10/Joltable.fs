namespace AdventOfCode2020.Day10

module Joltable =

    type Joltable =
        | Outlet
        | Adapter of Output:int
        | Device of MaxInput:int

        member this.Output =
            match this with
            | Outlet _ -> 0
            | Adapter output -> output
            | Device maxInput -> maxInput

        member this.MinInput =
            match this with
            | Outlet _ -> 0
            | Adapter output -> output - 3
            | Device maxInput -> maxInput - 3
         
        member this.MaxInput =
            match this with
            | Outlet _ -> 0
            | Adapter output -> output - 1
            | Device maxInput -> maxInput 