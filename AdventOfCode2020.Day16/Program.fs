namespace AdventOfCode2020.Day16

open System
open System.IO
open System.Text.RegularExpressions
open AdventOfCode2020.Day16.Train

module Program = 

    type Input = 
        {
            Fields: list<Field>
            MyTicket: Ticket
            NearbyTickets: list<Ticket>
        }

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

    [<EntryPoint>]
    let main argv =
        let input = readFile argv.[0]

        calcPart1Answer input
        |> printfn "The answer to part 1 is '%d'."

        0