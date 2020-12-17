namespace AdventOfCode2020.Day14

module Bitmask =

    type Instruction =
        | SetMask of Value:list<char>
        | WriteToMem of Address:int * Value:uint64