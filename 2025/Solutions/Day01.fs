namespace _2025.Solutions

open System

module Day01 =
    let parseRotations (line:string) : int =
        let direction = line[0]
        let clicks = Int32.Parse(line[1..])
        match direction with
        | 'L' -> -clicks
        | 'R' -> +clicks
        | _ -> failwithf "Invalid direction: %c" direction

    let parseInput (input:string) : int list =
        input.Split('\n')
        |> Array.toList
        |> List.map parseRotations

    let countEndingZeroPosition (position, zeroes) clicks =
        let position = (position + clicks) % 100
        let zeroes = if position = 0 then zeroes + 1 else zeroes
        position, zeroes

    let countPassedZeroPosition (position, zeroes) clicks =
        match Int32.Sign clicks with
        | -1 ->
            let p = (100 - position) % 100 - clicks
            (100 - p % 100) % 100, zeroes + p / 100
        | +1 ->
            let p = position + clicks
            p % 100, zeroes + p / 100
        | _ ->
            position, zeroes

    let countZeroPositions (rotateByClicks: int * int -> int -> int * int) (rotations: int list) : int =
        let (_, zeroes) = List.fold rotateByClicks (50, 0) rotations
        zeroes

    let part1 (input:string) : int =
        countZeroPositions countEndingZeroPosition (parseInput input)

    let part2 (input:string) : int =
        countZeroPositions countPassedZeroPosition (parseInput input)

    let run =
        let input = System.IO.File.ReadAllText("2025/Inputs/01/input.txt").TrimEnd()
        printfn "Day 01 - Part 1: %d" (part1 input)
        printfn "Day 01 - Part 2: %d" (part2 input)
