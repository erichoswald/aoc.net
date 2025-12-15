module Day05Tests

open Expecto
open _2025.Solutions

[<Tests>]
let tests =
    testList "Day 05 Tests" [
        let parseInput = Day05.parseInput (System.IO.File.ReadAllLines("2025/Samples/05/sample.txt"))

        testCase "parseRange parses range correctly" <| fun _ ->
            Expect.equal (Day05.parseRange "1-3") (1L, 3L) "Should parse range correctly"
        
        testCase "parseInput parses sample input correctly" <| fun _ ->
            let ranges, ids = parseInput
            Expect.equal ranges.Length 4 "Expected 4 ranges in sample input"
            Expect.equal ranges[1] (10L, 14L) "Expected range 2 to be (10, 14)"
            Expect.equal ids.Length 6 "Expected 6 ids in sample input"
            Expect.equal ids[0] 1 "Expected first id to be 1"
            Expect.equal ids[5] 32 "Expected first id to be 32"
        
        testTheory "isFresh determines freshness correctly" [1, false; 5, true; 17, true] <| fun (id, expected) ->
            let ranges = parseInput |> fst
            Expect.equal (Day05.isFresh ranges id) expected $"Expected ${id} to have freshness %b{expected}"
        
        testCase "countFreshIds counts fresh ids" <| fun _ ->
            let ranges, ids = parseInput
            Expect.equal (Day05.countFreshIds ranges ids) 3 "Expected 3 fresh ids"
            
        testCase "mergeRanges merges sample ranges" <| fun _ ->
            let ranges = parseInput |> fst
            let result = Day05.mergeRanges ranges
            Expect.equal result [3L, 5L; 10L, 20L] "Expected merged ranges to be [3, 5; 10, 20]"
        
        testCase "sumRanges sums sample ranges" <| fun _ ->
            let ranges = parseInput |> fst
            let result = Day05.mergeRanges ranges |> Day05.sumRanges
            Expect.equal result 14 "Expected sum of ranges to be 14"
    ]