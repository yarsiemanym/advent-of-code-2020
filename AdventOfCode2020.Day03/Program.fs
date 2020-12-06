module AdventOfCode2020.Day03

open System.IO

type Point = { 
    X: int
    Y: int
}

let parseLine line = Seq.toList line

let parseLines lines =
    [ 
        for line in lines do
            yield parseLine line
    ]

let readFile = 
    File.ReadAllLines 
    >> parseLines

let heightOf (map: list<list<char>>) = map.Length

let widthOf (map: list<list<char>>) = map.[0].Length

let check (point, map: list<list<char>>) = map.[point.Y].[point.X % widthOf map]

let constructPath (slope, map: list<list<char>>) =
    (
        seq {
            for i in 0 .. (heightOf map / slope.Y) - 1 do
                yield { 
                    X = i * slope.X
                    Y = i * slope.Y
                }
         },
         map
    )

let isTree (point, map: list<list<char>>) = check (point, map) = '#'

let findTreesOnPath (path, map: list<list<char>>) =
    seq {
        for point in path do
            if isTree (point, map) then yield 'X' else yield 'O'
    }

let countTrees = Seq.filter ((=) 'X') >> Seq.length

let printAnswer answer = printfn "The answer is '%u'." answer

let findAnswer =
    constructPath 
    >> findTreesOnPath
    >> countTrees

[<EntryPoint>]
let main argv =
    let slopes =
        [ 
            { X = 1; Y = 1 }
            { X = 3; Y = 1 }
            { X = 5; Y = 1 }
            { X = 7; Y = 1 }
            { X = 1; Y = 2 }
        ]

    let map = readFile argv.[0]

    let treeCounts =
        [ 
            for slope in slopes do
                yield findAnswer (slope, map)
        ]

    treeCounts |> List.fold (*) 1 |> printAnswer
    0
