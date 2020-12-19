namespace AdventOfCode2020.Day16

open System
open System.IO
open System.Text.RegularExpressions
open AdventOfCode2020.Day16.Train

module Program = 

    type FieldAnalysis =
        {
            FieldName:string
            PossibleIndices:list<int>
        }

        member this.LockedIn = this.PossibleIndices.Length = 1

    type Input = 
        {
            Fields: list<Field>
            MyTicket: Ticket
            NearbyTickets: list<Ticket>
        }

        member this.ValidTickets = List.filter (fun (ticket:Ticket) -> ticket.IsValid this.Fields) this.NearbyTickets

        member this.AnalyzeFields = 
            [
                for field in this.Fields do
                    yield  
                        {
                            FieldName = field.Name
                            PossibleIndices =
                                [
                                    for i in 0 .. this.ValidTickets.[0].Values.Length - 1 do
                                        if List.forall (fun (ticket:Ticket) -> field.IsValid ticket.Values.[i]) this.ValidTickets then
                                            yield i
                                ]
                        }
            ]

    let parseTicketSection (text:string) =
        let lines = text.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        parseTickets lines.[1..]

    let parseInput text =
        let pattern = @"(\s*\n){2,}"
        let sections =
            Regex.Split(text, pattern)
            |> Array.filter (not << String.IsNullOrWhiteSpace)

        if sections.Length = 3 then
            {
                Fields = parseFields sections.[0]
                MyTicket = parseTicketSection sections.[1] |> List.head
                NearbyTickets = parseTicketSection sections.[2]
            }
        else
            failwithf "Invalid input."

    let readFile =
        File.ReadAllText
        >> parseInput

    let calcPart1Answer input = List.sumBy (fun (ticket:Ticket) -> ticket.ErrorRate input.Fields) input.NearbyTickets   

    let lockInNextField analysis = 
        let lockedIndices = List.choose (fun (field:FieldAnalysis) -> if field.LockedIn then Some field.PossibleIndices.Head else None) analysis
        [
            for field in analysis do
                if field.LockedIn then
                    yield field
                else
                    yield { field with PossibleIndices = List.except lockedIndices field.PossibleIndices }
        ]

    let identifyFields (initialAnalysis:list<FieldAnalysis>) = 
        let mutable progressiveAnalysis = initialAnalysis
        let mutable continueLoop = true

        while continueLoop do
            progressiveAnalysis <- lockInNextField progressiveAnalysis
            continueLoop <- List.exists (fun (field:FieldAnalysis) -> not field.LockedIn) progressiveAnalysis

        List.map (fun (field:FieldAnalysis) -> (field.FieldName, field.PossibleIndices.Head)) progressiveAnalysis        

    let calcPart2Answer myTicket = 
        List.choose (fun (field:string*int) -> if (fst field).StartsWith("departure") then Some (uint64 myTicket.Values.[snd field]) else None)
        >> List.fold (*) 1UL

    [<EntryPoint>]
    let main argv =
        let input = readFile argv.[0]

        calcPart1Answer input
        |> printfn "The answer to part 1 is '%d'."

        input.AnalyzeFields
        |> identifyFields
        |> calcPart2Answer input.MyTicket
        |> printfn "The answer to part 2 is '%d'."

        0