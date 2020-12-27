namespace AdventOfCode2020.Day19

open System.Text.RegularExpressions

module Rules =

    type Rule =
        | SimpleRule of Id:int * Character:char
        | CompositeRule of Id:int * Subrules:list<list<int>>

        member this.Id =
            match this with
            | SimpleRule(id, _) -> id
            | CompositeRule(id, _) -> id

    let rec validate (rules:Map<int, Rule>) ruleId (message:string) =
        match rules.[ruleId] with
        | SimpleRule(_, character) -> 
            if message.[0] = character then 
                [ sprintf "%c" character ]
            else 
                List.Empty
        | CompositeRule(_, subrules) ->
            [
                for subrule in subrules do
                    let mutable validatedPortions = [ "" ]
                    let mutable isValid = true
                    let mutable i = 0

                    while isValid && i < subrule.Length do
                        let ruleId = subrule.[i]

                        validatedPortions <- 
                            [

                                for validatedPortion in validatedPortions do
                                    let unvalidatedPortion = message.[validatedPortion.Length .. message.Length - 1]

                                    if unvalidatedPortion = "" then
                                        isValid <- false
                                    else
                                        let results = validate rules ruleId unvalidatedPortion

                                        if List.isEmpty results then
                                            isValid <- false
                                        else
                                            for result in results do
                                                yield sprintf "%s%s" validatedPortion result        
                            ]

                        i <- i + 1
                    
                    yield! validatedPortions
            ]
            
    let parseSubrule text =
        let subrulePattern = @"^((\d+) ?)+|$"
        let m = Regex.Match(text, subrulePattern)

        if m.Success then
            [
                for capture in m.Groups.[2].Captures do
                    yield int capture.Value
            ]
        else
            failwithf "Invalid subrule '%s'." text

    let parseRule text =
        let simplePattern = @"^(\d+): ""([a-z])""$"
        let simpleMatch = Regex.Match(text, simplePattern)

        if simpleMatch.Success then
            let id = int simpleMatch.Groups.[1].Value
            let character = simpleMatch.Groups.[2].Value.[0]
            SimpleRule(id, character)
        else
            let compositePattern = @"^(\d+): (((\d+) ?)+(\| )?)+$"
            let compositeMatch = Regex.Match(text, compositePattern)

            if compositeMatch.Success then
                let id = int compositeMatch.Groups.[1].Value
                CompositeRule(id,
                    Subrules =
                        [
                            for capture in compositeMatch.Groups.[2].Captures do
                                yield parseSubrule capture.Value
                        ]
                    )
            else
                failwithf "Invalid rule '%s'." text
        

    let parseRules lines = 
        Map.ofList
            [
                for line in lines do
                    let rule = parseRule line
                    yield (rule.Id, rule)
            ]