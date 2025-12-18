namespace _2025.Solutions

open System

module Day12 =
    type Pattern = {
        Occupied: int
        Empty: int
    }
    
    type Region = {
        Width: int
        Height: int
        Patterns: int[]
    }
    
    let parseInput (lines: string[]) =
        let mutable patterns = ResizeArray<Pattern>()
        let mutable regions = ResizeArray<Region>()
        let mutable line = 0
        while not (lines[line].Contains('x')) do
            line <- line + 1
            let mutable occupied, empty = 0, 0
            while lines[line].Length > 0 do
                for ch in lines[line] do
                    match ch with
                    | '#' -> occupied <- occupied + 1
                    | '.' -> empty <- empty + 1
                    | _ -> failwith $"unexpected character: {ch}"
                line <- line + 1
            patterns.Add({ Occupied = occupied; Empty = empty })
            line <- line + 1
        while line < lines.Length do
            let parts = lines[line].Split(':')
            let dimensions = parts[0].Split('x') |> Array.map int
            let patterns = parts[1].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int
            regions.Add({ Width = dimensions[0]; Height = dimensions[1]; Patterns = patterns })
            line <- line + 1
        patterns.ToArray(), regions.ToArray()

    let couldFit (patterns: Pattern[]) (region: Region) =
        let available = region.Width * region.Height
        let occupied = region.Patterns |> Array.mapi (fun i n -> patterns[i].Occupied * n) |> Array.sum
        available >= occupied
    
    let countCouldFit (patterns: Pattern[]) (regions: Region[]) =
        regions |> Array.sumBy (fun region -> if couldFit patterns region then 1 else 0)

    let run =
        let lines = System.IO.File.ReadAllLines("2025/Inputs/12/input.txt")
        let patterns, regions = parseInput lines
        let result, millis = Measure.measure (fun () -> countCouldFit patterns regions)
        printfn $"Day 12 - Part 1: %d{result} (took %.3f{millis}ms)"
