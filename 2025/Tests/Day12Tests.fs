module Day12Tests

open Expecto
open _2025.Solutions

[<Tests>]
let tests =
    testList "Day 12 Tests" [
        testCase "parseInput parses sample input" <| fun _ ->
            let patterns, regions = System.IO.File.ReadAllLines("2025/Samples/12/sample.txt") |> Day12.parseInput
            let samplePattern: Day12.Pattern = { Occupied = 7; Empty = 2 }
            let sampleRegions: Day12.Region[] = [|
                { Width = 4; Height = 4; Patterns = [|0; 0; 0; 0; 2; 0|] }
                { Width = 12; Height = 5; Patterns = [|1; 0; 1; 0; 2; 2|] }
                { Width = 12; Height = 5; Patterns = [|1; 0; 1; 0; 3; 2|] }
            |]
            Expect.equal patterns (Array.create 6 samplePattern) "Patterns should all have same size and weight"
            Expect.equal regions sampleRegions "Regions should match sample"
    ]