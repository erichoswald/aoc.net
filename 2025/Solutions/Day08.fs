namespace _2025.Solutions

open System

module Day08 =
    type Position = int64 * int64 * int64

    type Connection = {
        box0: int
        box1: int
        distance: int64
    }

    let parsePosition (line: string) : Position =
        let coordinates = line.Split(',') |> Array.map Int64.Parse
        (coordinates[0], coordinates[1], coordinates[2])

    let parsePositions (lines: string seq) =
        lines |> Seq.map parsePosition |> Seq.toList
    
    let distance (p0: Position) (p1: Position) =
        let x0, y0, z0 = p0
        let x1, y1, z1 = p1
        let dx = x1 - x0
        let dy = y1 - y0
        let dz = z1 - z0
        dx * dx + dy * dy + dz * dz
    
    let buildConnections (positions: Position list) : Connection list =
        let unordered = [
            for box0 in 0 .. positions.Length - 1 do
                for box1 in box0 + 1 .. positions.Length - 1 do
                    let distance = distance (positions[box0]) (positions[box1])
                    yield { box0 = box0; box1 = box1; distance = distance }
        ]
        unordered |> List.sortBy _.distance

    let join (connection: Connection) (circuitIndex: int array) (circuitMembers: Set<int> array) =
        let targetCircuit = circuitIndex[connection.box0]
        let sourceCircuit = circuitIndex[connection.box1]
        if sourceCircuit <> targetCircuit then
            for m in circuitMembers[sourceCircuit] do
                circuitIndex[m] <- targetCircuit
            circuitMembers[targetCircuit] <- circuitMembers[targetCircuit] |> Set.union circuitMembers[sourceCircuit]
            circuitMembers[sourceCircuit] <- Set.empty
        
    let part1 (n: int) (positions: Position list) : int =
        let connections = buildConnections positions
        let circuitIndex = Array.init positions.Length id
        let circuitMembers = Array.init positions.Length Set.singleton
        for i in 0 .. n - 1 do
            join (connections[i]) circuitIndex circuitMembers

        circuitMembers
        |> Seq.ofArray
        |> Seq.map _.Count
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.fold (*) 1
        
    let run =
        let lines = System.IO.File.ReadAllLines "2025/Inputs/08/input.txt"
        let positions = parsePositions lines
        printfn $"Day 08 - Part 1: %d{part1 1000 positions}"
