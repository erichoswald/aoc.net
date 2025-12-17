module Day06Tests

open Expecto
open _2025.Solutions

[<Tests>]
let tests =
    testList "Day 06 Tests" [
        let sampleProblems = Day06.parseProblems (System.IO.File.ReadAllLines("2025/Samples/06/sample.txt"))
        
        testCase "parseProblems parses sample input" <| fun _ ->
            Expect.equal sampleProblems.Length 4 "Expected 4 problems"
            Expect.equal sampleProblems[0] { Width = 3; Operands = ["123"; " 45"; "  6"]; Operation = '*' } "Expected first problem to be 123 * 45 * 6"
            Expect.equal sampleProblems[3] { Width = 3; Operands = ["64 "; "23 "; "314"]; Operation = '+' } "Expected last problem to be 64 + 24 + 314"
        
        testTheory
            "evaluateCephalopodNumbers returns expected result for sample data"
            [
                ["123"; " 45"; "  6"], [356L; 24L; 1L]
                ["328"; "64 "; "98 "], [8L; 248L; 369L]
                [" 51"; "387"; "215"], [175L; 581L; 32L]
                ["64 "; "23 "; "314"], [4L; 431L; 623L]
            ]
            <| fun (problem, expected) ->
                let result = Day06.evaluateCephalopodNumbers problem |> Seq.toList
                Expect.equal result expected "Evaluation failed"

        testCase "sumStandardSolutions returns expected sum for sample data" <| fun _ ->
            let result = Day06.sumStandardSolutions sampleProblems
            Expect.equal result 4277556L "Expected sum to be 4277556"

        testCase "sumCephalopodSolutions returns expected sum for sample data" <| fun _ ->
            let result = Day06.sumCephalopodSolutions sampleProblems
            Expect.equal result 3263827L "Expected sum to be 3263827"
    ]
