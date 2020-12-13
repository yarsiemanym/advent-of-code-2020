namespace AdventOfCode2020.Day13

module Bus = 

    type Bus =
        {
            Id:int
        }

        member this.NextDepartureAfter time = this.Id * ((time / this.Id) + if time % this.Id = 0 then 0 else 1)

    let parseBus text = { Id = int text }

    let parseBuses (text:string) = 
        text.Split(',') 
        |> Array.toList
        |> List.filter (fun id -> id <> "x")
        |> List.map parseBus