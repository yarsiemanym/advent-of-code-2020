namespace AdventOfCode2020.Day17

module PocketUniverse =

    type Coordinate =
        {
            X:int
            Y:int
            Z:int
            W:int
        }

    type Cube = 
        {
            Location:Coordinate
            IsActive:bool
        }

    let parseCube x y z w symbol =
        {
            Location =
                {
                    X = x
                    Y = y
                    Z = z
                    W = w
                }
            IsActive = symbol = '#'
        }

    let parseCubes (lines:list<string>) = 
        [
            for y in 0 .. lines.Length - 1 do
                let line = lines.[y]
                
                for x in 0 .. line.Length - 1 do
                    let coordinate = { X = x; Y = y; Z = 0; W = 0 }
                    let isActive = line.[x] = '#'

                    yield (coordinate, { Location = coordinate; IsActive = isActive })
        ]

    type PocketUniverse3D =
        {
            Cubes:Map<Coordinate, Cube>
        }

        member this.NextStateOfCube coordinate =
            let thisCube = if this.Cubes.ContainsKey coordinate then this.Cubes.[coordinate] else { Location = coordinate; IsActive = false }
            let neighboringCoordinates = 
                [
                    for x in coordinate.X - 1 .. coordinate.X + 1 do
                        for y in coordinate.Y - 1 .. coordinate.Y + 1 do
                            for z in coordinate.Z - 1 .. coordinate.Z + 1 do
                                let neighboringCoordinate = { X = x; Y = y; Z = z; W = 0}
                                if neighboringCoordinate <> coordinate then
                                    yield neighboringCoordinate
                ]

            let activeNeighbors = 
                List.countBy (fun neighbor -> if this.Cubes.ContainsKey neighbor then this.Cubes.[neighbor].IsActive else false ) neighboringCoordinates
                |> List.filter fst
                |> List.map snd
                |> List.tryHead
                |> (fun option -> match option with | Some(count) -> count | None -> 0)

            (thisCube.IsActive && activeNeighbors >= 2 && activeNeighbors <= 3) || (not thisCube.IsActive && activeNeighbors = 3)
               

        member this.AdvanceBootCycle =
            let knownCoordinates = 
                this.Cubes
                |> Map.toList
                |> List.map fst
            
            let xRange = 
                knownCoordinates
                |> List.map (fun coordinate -> coordinate.X)
                |> List.distinct
                |> List.sortBy id    

            let yRange = 
                knownCoordinates
                |> List.map (fun coordinate -> coordinate.Y)
                |> List.distinct
                |> List.sortBy id

            let zRange = 
                knownCoordinates
                |> List.map (fun coordinate -> coordinate.Z)
                |> List.distinct
                |> List.sortBy id

            {
                Cubes = Map.ofList
                    [
                        for x in List.min xRange - 1 .. List.max xRange + 1 do
                            for y in List.min yRange - 1 .. List.max yRange + 1 do
                                for z in List.min zRange - 1 .. List.max zRange + 1 do
                                    let coordinate = { X = x; Y = y; Z= z; W = 0 }
                                    yield (coordinate, { Location = coordinate; IsActive = this.NextStateOfCube coordinate })
                    ]
            }

    let parsePocketUniverse3D (lines:list<string>) = 
        {
            PocketUniverse3D.Cubes = Map.ofList (parseCubes lines)     
        }

    type PocketUniverse4D =
        {
            Cubes:Map<Coordinate, Cube>
        }

        member this.NextStateOfCube coordinate =
            let thisCube = if this.Cubes.ContainsKey coordinate then this.Cubes.[coordinate] else { Location = coordinate; IsActive = false }
            let neighboringCoordinates = 
                [
                    for x in coordinate.X - 1 .. coordinate.X + 1 do
                        for y in coordinate.Y - 1 .. coordinate.Y + 1 do
                            for z in coordinate.Z - 1 .. coordinate.Z + 1 do
                                for w in coordinate.W - 1 .. coordinate.W + 1 do
                                    let neighboringCoordinate = { X = x; Y = y; Z = z; W = w}
                                    if neighboringCoordinate <> coordinate then
                                        yield neighboringCoordinate
                ]

            let activeNeighbors = 
                List.countBy (fun neighbor -> if this.Cubes.ContainsKey neighbor then this.Cubes.[neighbor].IsActive else false ) neighboringCoordinates
                |> List.filter fst
                |> List.map snd
                |> List.tryHead
                |> (fun option -> match option with | Some(count) -> count | None -> 0)

            (thisCube.IsActive && activeNeighbors >= 2 && activeNeighbors <= 3) || (not thisCube.IsActive && activeNeighbors = 3)
               

        member this.AdvanceBootCycle =
            let knownCoordinates = 
                this.Cubes
                |> Map.toList
                |> List.map fst
            
            let xRange = 
                knownCoordinates
                |> List.map (fun coordinate -> coordinate.X)
                |> List.distinct
                |> List.sortBy id    

            let yRange = 
                knownCoordinates
                |> List.map (fun coordinate -> coordinate.Y)
                |> List.distinct
                |> List.sortBy id

            let zRange = 
                knownCoordinates
                |> List.map (fun coordinate -> coordinate.Z)
                |> List.distinct
                |> List.sortBy id

            let wRange = 
                knownCoordinates
                |> List.map (fun coordinate -> coordinate.W)
                |> List.distinct
                |> List.sortBy id

            {
                Cubes = Map.ofList
                    [
                        for x in List.min xRange - 1 .. List.max xRange + 1 do
                            for y in List.min yRange - 1 .. List.max yRange + 1 do
                                for z in List.min zRange - 1 .. List.max zRange + 1 do
                                    for w in List.min wRange - 1 .. List.max wRange + 1 do
                                        let coordinate = { X = x; Y = y; Z= z; W = w }
                                        yield (coordinate, { Location = coordinate; IsActive = this.NextStateOfCube coordinate })
                    ]
            }

    let parsePocketUniverse4D (lines:list<string>) = 
        {
            PocketUniverse4D.Cubes = Map.ofList (parseCubes lines)     
        }