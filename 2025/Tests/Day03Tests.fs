module Day03Tests

open Expecto
open _2025.Solutions

let sampleLines = [
    "987654321111111"
    "811111111111119"
    "234234234234278"
    "818181911112111"
]

[<Tests>]
let tests =
    testList "Day 03 Tests" [
        testTheory
            "maximumJoltage 2 returns the maximum reachable joltage when choosing 2 batteries"
            [ 0, 98; 1, 89; 2, 78; 3, 92 ]
            <| fun (index, joltage) ->
                Expect.equal
                    (Day03.maximumJoltage 2 sampleLines[index])
                    joltage
                    $"Expected a largest joltage of %d{joltage}"

        testTheory
            "maximumJoltage 12 returns maximum reachable joltage when choosing 12 batteries"
            [ 0, 987654321111L; 1, 811111111119L; 2, 434234234278L; 3, 888911112111L ]
            <| fun (index, joltage) ->
                Expect.equal
                    (Day03.maximumJoltage 12 sampleLines[index])
                    joltage
                    $"Expected a largest joltage of %d{joltage}"

        testCase "Part 1" <| fun _ ->
              let expected = 357
              Expect.equal (Day03.part1 sampleLines) expected "Wrong result for part 1"

        testCase "Part 2" <| fun _ ->
              let expected = 3121910778619L
              Expect.equal (Day03.part2 sampleLines) expected "Wrong result for part 2"
    ]
