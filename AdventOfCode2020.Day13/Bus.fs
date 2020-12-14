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
                time + uint64 this.Index

        member this.NextSyncTime (other:Bus) (start:uint64) = 
            let a = List.minBy (fun bus -> bus.Index) [this; other]
            let b = List.maxBy (fun bus -> bus.Index) [this; other]
            let offset = b.Index - a.Index
            
            if not a.InService && not b.InService then
                start
            else if not a.InService then
                start / b.Id * b.Id
            else if not b.InService then
                start / a.Id * a.Id
            else    
                let mutable i = (start / a.Id) + if start % a.Id = 0UL then 0UL else 1UL
                let mutable continueLoop = true
                let mutable nextSyncTime = 0UL

                while continueLoop do
                    let product = a.Id * i

                    if (product + offset) % b.Id = 0UL then
                        nextSyncTime <- product
                        continueLoop <- false
                    
                    i <- i + 1UL

                nextSyncTime

    let parseBus (index:int) text =
        match text with
        | "x" -> { Index = uint64 index; InService = false; Id = 1UL }
        | _ -> { Index = uint64 index; InService = true; Id = uint64 text }

    let parseBuses (text:string) = 
        text.Split(',') 
        |> Array.toList
        |> List.mapi parseBus