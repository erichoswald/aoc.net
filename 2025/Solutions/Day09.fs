namespace _2025.Solutions

module Day09 =
    let parseCoordinatePair (line: string) =
        let coordinates = line.Split(',') |> Array.map System.Int64.Parse
        coordinates[0], coordinates[1]
    
    let parseCoordinates (lines: string[]) =
        lines |> Array.map parseCoordinatePair
    
    let area (corner: int64 * int64) (opposite: int64 * int64) =
        let width = abs(fst corner - fst opposite) + 1L
        let height = abs(snd corner - snd opposite) + 1L
        width * height

    let areas (coordinates: (int64 * int64) seq) =
        seq {
            for u in coordinates do
                for v in coordinates do
                    yield area u v
        }
        
    let largestRectangleArea (coordinates: (int64 * int64) seq) =
        areas coordinates |> Seq.max

    let run =
        let input = System.IO.File.ReadAllLines "2025/Inputs/09/input.txt"
        let coordinates = parseCoordinates input
        printfn $"Day 09 - Part 1: %d{largestRectangleArea coordinates}"