module Day06Tests

open Expecto
open _2025.Solutions

[<Tests>]
let tests =
    testList "Day 06 Tests" [
        let sampleProblems = Day06.parseProblems (System.IO.File.ReadAllLines("2025/Samples/06/sample.txt"))
        
        testCase "parseProblems parses sample input" <| fun _ ->
            Expect.equal sampleProblems.Length 4 "Expected 4 problems"
            Expect.equal sampleProblems[0] { numbers = [|123; 45; 6|]; operation = '*' } "Expected first problem to be 123 * 45 * 6"
            Expect.equal sampleProblems[3] { numbers = [|64; 23; 314|]; operation = '+' } "Expected last problem to be 64 + 24 + 314"
        
        testCase "sumSolutions returns expected sum for sample data" <| fun _ ->
            let result = Day06.sumSolutions sampleProblems
            Expect.equal result 4277556L "Expected sum to be 4277556"
    ]
