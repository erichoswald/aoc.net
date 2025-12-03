module Day03Tests

open Expecto
open _2025.Solutions

let sampleLines = [
    "987654321111111";
    "811111111111119";
    "234234234234278";
    "818181911112111";
]

[<Tests>]
let tests =
    testList "Day 03 Tests" [
        testTheory
            "largestJoltage returns maximum reachable joltage"
            [0, 98; 1, 89; 2, 78; 3, 92]
        <| fun (index, joltage) ->
            Expect.equal (Day03.largestJoltage (sampleLines[index])) joltage $"Expected %d{joltage}"

        testCase "Part 1" <| fun _ ->
            let expected = 357
            Expect.equal (Day03.part1 sampleLines) expected $"Expected %d{expected}"
    ]
