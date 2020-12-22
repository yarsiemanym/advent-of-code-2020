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

        member this.ValidCominations (rules:Map<int, Rule>) = 
            [
                match this with
                | SimpleRule(_, character) -> yield sprintf "%c" character
                | CompositeRule(_, subrules) -> 
                    yield! 
                        List.fold (fun agg subrule ->
                            agg @ List.fold (fun roots ruleId -> 
                                let rule = rules.[ruleId]
                                let combos = rule.ValidCominations rules
                                
                                [
                                    for combo in combos do
                                        for root in roots do
                                            yield sprintf "%s%s" root combo
                                ]
                            ) [""] subrule
                        ) [] subrules
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