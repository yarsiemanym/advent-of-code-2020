namespace AdventOfCode2020.Day04

open System.IO
open AdventOfCode2020.Day04.Passport

module Program =

    let isValidPassportPart1 passport = hasRequiredFields passport.Fields

    let isValidPassportPart2 passport = hasRequiredFields passport.Fields && hasValidFieldValues passport.Fields

    let readFile =
        File.ReadAllText 
        >> parsePassports

    let inspectPassports passports validator =
        [
            for passport in passports do
                yield validator passport
        ]

    let findAnswer passports validator =
        inspectPassports passports validator
        |> List.filter ((=) true)
        |> List.length

    [<EntryPoint>]
    let main argv =
        let passports = readFile argv.[0]

        findAnswer passports isValidPassportPart1
        |> printfn "The answer to part 1 is '%d'."

        findAnswer passports isValidPassportPart2
        |> printfn "The answer to part 2 is '%d'."

        0
