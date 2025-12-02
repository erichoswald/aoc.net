module Day01Tests

open Expecto
open _2025.Solutions

[<Tests>]
let tests =
    testList "Day 01 Tests" [
        testCase "parseInput parses rotations correctly" <| fun _ ->
            let input = "L3\nR5\nL10\nR20"
            let result = Day01.parseInput input
            let expected = [-3; +5; -10; +20]
            Expect.equal result expected "Rotations should be parsed correctly"

        testCase "part1 processes sample input correctly" <| fun _ ->
            let input = System.IO.File.ReadAllText("2025/Samples/01/sample.txt").TrimEnd()
            let result = Day01.part1 input
            Expect.equal result 3 "Part 1 should return 3 zero positions"

        testCase "part2 processes sample input correctly" <| fun _ ->
            let input = System.IO.File.ReadAllText("2025/Samples/01/sample.txt").TrimEnd()
            let result = Day01.part2 input
            Expect.equal result 6 "Part 2 should return 6 zero positions"
    ]
