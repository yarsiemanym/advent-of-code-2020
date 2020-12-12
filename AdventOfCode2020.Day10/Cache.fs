namespace AdventOfCode2020.Day10

open System.Collections.Generic
open System.Linq
open AdventOfCode2020.Day10.Joltable

module Cache =

    let private cache = new Dictionary<Joltable, uint64>()

    let checkCache source = 
        if cache.Keys.Contains(source) then
            (true, cache.[source])
        else
            (false, 0UL)

    let addToCache source count = cache.[source] <- count