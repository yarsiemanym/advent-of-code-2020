namespace AdventOfCode2020.Day12

module Point = 

    type Point =
        {
            X:int
            Y:int
        }

        member this.ManhattanDistance point = (abs (point.X - this.X)) + (abs (point.Y - this.Y))