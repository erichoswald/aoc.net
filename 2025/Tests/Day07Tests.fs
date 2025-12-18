module Day07Tests

open Expecto
open _2025.Solutions

[<Tests>]
let tests =
    let rows = System.IO.File.ReadAllLines("2025/Samples/07/sample.txt")

    testList "Day 07 Tests" [
        testCase "split starts a beam at S" <| fun _ ->
            let result = Day07.split (0, Set.empty) ".S."
            Expect.equal result (0, Set([1])) "Expected beam in column 1"
        
        testCase "split splits a beam in two" <| fun _ ->
            let result = Day07.split (0, Set([1]))  ".^."
            Expect.equal result (1, Set([0; 2])) "Expected beams in columns 0 and 2"
        
        testCase "split splits two beams in three" <| fun _ ->
            let result = Day07.split (1, Set([1; 3]))  ".^.^."
            Expect.equal result (3, Set([0; 2; 4])) "Expected beams in columns 0, 2 and 4"
        
        testCase "split many beams into many" <| fun _ ->
            let result = Day07.split(16, Set[1; 3; 4; 5; 7; 8; 10; 11; 13]) ".^.^.^.^.^...^."
            Expect.equal result (21, Set[0; 2; 4; 6; 8; 10; 11; 12; 14]) "Expected beams in last row"
        
        testCase "splitBeams traces beams through sample input" <| fun _ ->
            let result = Day07.splitBeams rows
            Expect.equal result 21 "Expected 21 beams after sending beams through the sample input"
        
        testCase "countTimelines counts tachyon beams through sample input" <| fun _ ->
            let result = Day07.countTimelines rows
            Expect.equal result 40 "Expected 40 timelines in the sample input"
    ]