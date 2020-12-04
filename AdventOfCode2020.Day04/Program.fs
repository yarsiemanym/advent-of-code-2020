module AdventOfCode2020.Day04

open System
open System.IO
open System.Text.RegularExpressions

type Field = { Name:string; Value:string }
type Passport = { Fields:list<Field>}

let requiredFields = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]

let contains (fields, fieldName) = 
    let contains = 
        List.filter (fun f -> f.Name = fieldName) fields 
        |> List.length 
        |> (<) 0
    contains 

let hasRequiredFields fields = List.fold (fun v f -> v && contains (fields, f)) true requiredFields

let isValidByr byr = 1920 <= byr && byr <= 2002

let isValidIyr iyr = 2010 <= iyr && iyr <= 2020

let isValidEyr eyr = 2020 <= eyr && eyr <= 2030

let isValidHgt (hgt:string) = 
    if hgt.Length < 3 then
        false
    else
        let value = int hgt.[0 .. hgt.Length - 3]
        let unit = hgt.[hgt.Length - 2 .. hgt.Length - 1]

        match unit with
        | "cm" -> 150 <= value && value <= 193
        | "in" -> 59 <= value && value <= 76
        | _ -> false

let isValidHcl (hcl:string) = Regex.IsMatch (hcl, @"^#[a-f0-9]{6}$")

let isValidEcl (ecl:string) = 
    match ecl with
    | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
    | _ -> false

let isValidPid (pid:string) = Regex.IsMatch(pid, @"^\d{9}$")

let isValidField field =
    match field.Name with
    | "byr" -> isValidByr (int field.Value)
    | "iyr" -> isValidIyr (int field.Value)
    | "eyr" -> isValidEyr (int field.Value)
    | "hgt" -> isValidHgt field.Value
    | "hcl" -> isValidHcl field.Value
    | "ecl" -> isValidEcl field.Value
    | "pid" -> isValidPid field.Value
    | _ -> true


let hasValidFieldValues fields = List.fold (fun v f -> v && isValidField f) true fields

let isValidPassport passport = hasRequiredFields passport.Fields && hasValidFieldValues passport.Fields

let parseField text = 
    let tokens = Regex.Split(text, @":")

    if tokens.Length = 2 then 
        { Name = tokens.[0]; Value = tokens.[1] }
    else
        raise (Exception (sprintf "Malformed field '%s'" text))

let parseFields text =
    let fields = 
        Regex.Split(text, @"\s+", RegexOptions.Singleline) 
        |> Array.filter (not << String.IsNullOrWhiteSpace)

    [
        for field in fields do
            yield parseField field
    ]

let parsePassports text =
    let passports = 
        Regex.Split(text, @"(\s*\n){2,}", RegexOptions.Singleline) 
        |> Array.filter (not << String.IsNullOrWhiteSpace)

    [
        for passport in passports do
            yield { Fields = parseFields passport }
    ]

let readFile =
    File.ReadAllText 
    >> parsePassports

let inspectPassports passports =
    [
        for passport in passports do
            yield isValidPassport passport
    ]

let printAnswer answer = printfn "The answer is '%d'." answer

let findAnswer =
    readFile
    >> inspectPassports
    >> List.filter ((=) true)
    >> List.length
    >> printAnswer

[<EntryPoint>]
let main argv =
    findAnswer argv.[0]
    0
