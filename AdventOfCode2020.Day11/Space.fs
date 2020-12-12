namespace AdventOfCode2020.Day11

module Space = 

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