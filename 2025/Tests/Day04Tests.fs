module Day04Tests

open Expecto
open _2025.Solutions

[<Tests>]
let tests =
    testList "Day 04 Tests" [
        testCase "countAdjacentRolls on single cell" <| fun _ ->
            let input = array2D [| "@" |]
            Expect.equal (Day04.countAdjacentRolls input 0 0) 0 "Unexpected number of adjacent rolls"

        testCase "countAdjacentRolls on full grid" <| fun _ ->
            let input = array2D [| "@@@"; "@@@"; "@@@" |]
            Expect.equal (Day04.countAdjacentRolls input 1 1) 8 "Unexpected number of adjacent rolls"

        testCase "countAdjacentRolls on partially full grid" <| fun _ ->
            let input = array2D [| "@.@"; "@@."; ".@@" |]
            Expect.equal (Day04.countAdjacentRolls input 1 1) 5 "Unexpected number of adjacent rolls"
        
        testCase "part 1 computes correct result on sample input" <| fun _ ->
            let grid = Day04.parseInput "2025/Samples/04/sample.txt"
            let result = Day04.countAccessibleRolls grid
            Expect.equal result 13 "Unexpected result for part 1"
        
        testCase "part 2 computes correct result on sample input" <| fun _ ->
            let grid = Day04.parseInput "2025/Samples/04/sample.txt"
            let result = Day04.removeAllAccessibleRolls grid
            Expect.equal result 43 "Unexpected result for part 2"
    ]
