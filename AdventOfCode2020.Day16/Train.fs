namespace AdventOfCode2020.Day16

open System
open System.Text.RegularExpressions

module Train =

    type Field =
        {
            Name:string
            Ranges:list<int*int>
        }

        member this.IsValid value = List.exists (fun range -> fst range <= value && value <= snd range ) this.Ranges

    type Ticket =
        {
            Values:list<int>
        }

        member this.IsValid fields = List.forall (fun value -> List.exists (fun (field:Field) -> field.IsValid value) fields) this.Values

        member this.ErrorRate fields = List.sumBy (fun value -> if not <| List.exists (fun (field:Field) -> field.IsValid value) fields then value else 0) this.Values

    let parseRange text = 
        let pattern = @"(\d+)-(\d+)"
        let m = Regex.Match(text, pattern)

        if m.Success then
            (int m.Groups.[1].Value, int m.Groups.[2].Value)
        else 
            failwithf "Invalid range '%s'." text

    let parseField text = 
        let pattern = @"^([\w\s]+):( \d+-\d+( or)?)+$"
        let m = Regex.Match(text, pattern)

        if m.Success then
            {
                Name = m.Groups.[1].Value
                Ranges = 
                    [
                        for capture in m.Groups.[2].Captures do
                            yield parseRange capture.Value
                    ]
            }
        else
            failwithf "Invalid field '%s'." text


    let parseFields (text:string) =
        let lines = text.Split('\n', StringSplitOptions.RemoveEmptyEntries)

        [
            for line in lines do
                yield parseField line
        ]

    let parseTicket (text:string) =
        {
            Values = text.Split(',', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (int)
                |> Array.toList
        }

    let parseTickets lines =
        [
            for line in lines do
                yield parseTicket line
        ]