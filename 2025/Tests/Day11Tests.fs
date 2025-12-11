module Day11Tests

open Expecto
open _2025.Solutions

[<Tests>]
let tests =
    let sample1Devices = Day11.parseDevices (System.IO.File.ReadAllLines("2025/Samples/11/sample1.txt"))

    testList "Day 11 Tests" [
        testCase "parseDevice parses device correctly" <| fun _ ->
            let result = Day11.parseDevice "aaa: you hhh"
            let expected: Day11.Device = { Name = "aaa"; Outputs = ["you"; "hhh"] }
            Expect.equal result expected "Failed to parse device"
        
        testCase "countPaths counts sample paths correctly" <| fun _ ->
            let result = Day11.countPaths sample1Devices
            Expect.equal result 5 "Failed to count paths"
    ]