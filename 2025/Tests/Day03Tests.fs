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
            "weakestIndex returns index of weakest element"
            [
                [ ], 0L, None;
                [ 3L ], 2L, None;
                [ 1L ], 2L, Some 0;
                [ 1L; 2L ], 2L, Some 0;
                [ 3L; 4L; 5L ], 1L, Some 0;
                [ 4L; 3L; 2L; 3L; 4L ], 3L, Some 2;
            ]
            <| fun (selection, next, expected) ->
                let actual = Day03.weakestIndex selection 0 next
                Expect.equal actual expected $"Expected weakest index of %A{selection} with %d{next} to be different"

        testCase "maximize replaces the weakest element if it exists" <| fun _ ->
            let input = [ 0L; 4L; 5L; 2L; 8L ]
            let result = Day03.maximize input 7L
            Expect.equal result [ 4L; 5L; 2L; 8L; 7L ] "Expected maximize to replace the weakest element"

        testCase "maximize has no effect if there is no element weaker than the next candidate" <| fun _ ->
            let input = [ 9L; 9L ]
            let result = Day03.maximize input 1L
            Expect.equal result [ 9L; 9L ] "Expected maximize to have no effect"

        testCase "joltage computes the resulting joltage" <| fun _ ->
            Expect.equal (Day03.joltage [ 0L; 1L; 2L; 3L ]) 123L "Expected joltage to accumulate digit values"

        testTheory
            "largestJoltage 2 returns the maximum reachable joltage"
            [ 0, 98; 1, 89; 2, 78; 3, 92 ]
            <| fun (index, joltage) ->
                Expect.equal
                    (Day03.largestJoltage 2 (sampleLines[index]))
                    joltage
                    $"Expected a largest joltage of %d{joltage}"

        testTheory
            "largestJoltage 12 returns maximum reachable joltage"
            [ 0, 987654321111L; 1, 811111111119L; 2, 434234234278L; 3, 888911112111L ]
            <| fun (index, joltage) ->
                Expect.equal
                    (Day03.largestJoltage 12 (sampleLines[index]))
                    joltage
                    $"Expected a largest joltage of %d{joltage}"

        testCase "Part 1" <| fun _ ->
              let expected = 357
              Expect.equal (Day03.part1 sampleLines) expected "Wrong result for part 1"

        testCase "Part 2" <| fun _ ->
              let expected = 3121910778619L
              Expect.equal (Day03.part2 sampleLines) expected "Wrong result for part 2"
    ]
