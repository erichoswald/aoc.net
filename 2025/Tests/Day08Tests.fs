module Day08Tests

open Expecto
open _2025.Solutions

[<Tests>]
let tests =
    testList "Day 08 Tests" [
        let samplePositions = Day08.parsePositions (System.IO.File.ReadAllLines("2025/Samples/08/sample.txt"))

        testCase "parsePositions returns parsed positions" <| fun _ ->
            Expect.equal samplePositions.Length 20 "Expected 20 parsed positions"
            Expect.equal samplePositions[0] (162L, 817L, 812L) "Expected first position to be (162, 817, 812)"
            Expect.equal samplePositions[19] (425L, 690L, 689L) "Expected last position to be (425, 690, 689)"
        
        testCase "buildConnections puts shortest distance at first position" <| fun _ ->
            let connections = Day08.buildConnections samplePositions
            let expected: Day08.Connection = { box0 = 0; box1 = 19; distance = 263L * 263L + 127L * 127L + 123L * 123L }
            Expect.equal connections[0] expected "Expected closest distance to be between first and last position"
        
        testTheory
            "part1 finds largest circuits after each iteration"
            [ 1, [2; 1; 1]; 2, [3; 1; 1]; 3, [3; 2; 1]; 4, [3; 2; 1]; 10, [5; 4; 2] ]
            <| fun (iterations, sizes) ->
                let result = Day08.part1 iterations samplePositions
                let expected = List.fold (*) 1 sizes
                Expect.equal result expected $"Expected correct largest circuits to have {sizes}"
        
        testCase "part2 multiplies x coordinates of connection that achieves single circuit" <| fun _ ->
            let result = Day08.part2 samplePositions
            Expect.equal result 25272L "Expected correct result"
    ]
