namespace AdventOfCode2020.Day13

module Math = 

    let rec greatestCommonDemoniator (a:uint64) (b:uint64) =
        if b = 0UL then   
            a
        else greatestCommonDemoniator b (a % b)
    
    let leastCommonMultiple2 (a:uint64) (b:uint64) = a * b / (greatestCommonDemoniator a b)

    let leastCommonMultipleN = List.fold leastCommonMultiple2 1UL