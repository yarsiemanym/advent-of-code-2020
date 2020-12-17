namespace AdventOfCode2020.Day13

module Bus = 

    type Bus =
        {
            Index:uint64
            InService:bool
            Id:uint64
        }

        member this.NextDeparture time =
            if this.InService then
                this.Id * ((time / this.Id) + if time % this.Id = 0UL then 0UL else 1UL)
            else
                time + 1UL

    let parseBus (index:int) text =
        match text with
        | "x" -> { Index = uint64 index; InService = false; Id = 1UL }
        | _ -> { Index = uint64 index; InService = true; Id = uint64 text }

    let parseBuses (text:string) = 
        text.Split(',') 
        |> Array.toList
        |> List.mapi parseBus