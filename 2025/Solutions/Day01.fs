namespace _2025.Solutions

open System

module Day01 =
    type Rotation =
        | Left of clicks : int
        | Right of clicks : int

    let parseLines (lines:List<string>) : Rotation list =
        lines
        |> List.map (fun line ->
            let direction = line.[0]
            let clicks = Int32.Parse(line.[1..])
            match direction with
            | 'L' -> Left clicks
            | 'R' -> Right clicks
            | _ -> failwithf "Invalid direction: %c" direction
        )

    let countFinalZeroPositions (rotations: Rotation list) : int =
        let (_, zeroes) = List.fold (fun (position, zeroes) rotation -> 
            let newPosition =
                match rotation with
                | Left clicks -> (position - clicks) % 100
                | Right clicks -> (position + clicks) % 100
            let newZeroes =
                if newPosition = 0 then zeroes + 1 else zeroes
            (newPosition, newZeroes)) (50, 0) rotations
        zeroes

    let countZeroPositions (rotations: Rotation list) : int =
        let mutable zeroes = 0
        let mutable position = 50
        for rotation in rotations do
            match rotation with
            | Left clicks ->
                let p = (100 - position) % 100 + clicks
                zeroes <- zeroes + p / 100
                position <- (100 - p % 100) % 100
            | Right clicks ->
                let p = position + clicks
                zeroes <- zeroes + p / 100
                position <- p % 100
        zeroes

    let part1 (input:string) : string =
        let lines = input.Split('\n') |> Array.toList
        let rotations = parseLines lines
        let count = countFinalZeroPositions rotations
        string count

    let part2 (input:string) : string =
        let lines = input.Split('\n') |> Array.toList
        let rotations = parseLines lines
        let count = countZeroPositions rotations
        string count

    let run =
        let input = System.IO.File.ReadAllText("2025/Inputs/01/input.txt").TrimEnd()
        printfn "Day 01 - Part 1: %s" (part1 input)
        printfn "Day 01 - Part 2: %s" (part2 input)
