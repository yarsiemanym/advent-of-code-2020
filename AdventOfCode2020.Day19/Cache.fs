namespace AdventOfCode2020.Day19

open System.Collections.Generic
open System.Linq
open AdventOfCode2020.Day19.Rules

module Cache =

    let private cache = new Dictionary<Rule, list<string>>()

    let checkCache rule = 
        if cache.Keys.Contains(rule) then
            (true, cache.[rule])
        else
            (false, List.empty)

    let addToCache rule validCombinations = cache.[rule] <- validCombinations