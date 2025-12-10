module Day10Tests

open Expecto
open _2025.Solutions

let sampleLines = [|
    "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
    "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
    "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
|]

[<Tests>]
let tests =
    let machines = Day10.parseMachines sampleLines

    testList "Day 10 Tests" [
        testTheory
            "parseLights parses indicator light codes correctly"
            [".##.", 0b0110; "...#.", 0b01000; ".###.#", 0b101110]
            <| fun (input, expected) ->
                Expect.equal (Day10.parseLights input) expected "Expected parsed light code"
        
        testTheory
            "parseButton parses button code correctly"
            ["3", 0b1000; "1,3", 0b1010; "2", 0b100; "2,3", 0b1100; "0,2", 0b101]
            <| fun (input, expected) ->
                Expect.equal (Day10.parseButton input) expected "Expected parsed button code"
        
        testCase "parseButtons parses button codes correctly" <| fun _ ->
            let result = Day10.parseButtons "(1,3) (2) (2,3)"
            Expect.equal result [|0b1010; 0b100; 0b1100|] "Expected parsed button codes"
            
        testCase "parsed machines contain expected values" <| fun _ ->
            Expect.equal machines.Length 3 "Expected 3 machines"
            let machine: Day10.Machine = { Lights = 0b0110; Buttons = [|0b1000; 0b1010; 0b100; 0b1100; 0b101; 0b11|] }
            Expect.equal machines[0] machine "Expected first machine to match sample"
        
        testTheory
            "fewestTotalPresses returns expected value"
            [ 0, 2; 1, 3; 2, 2 ]
            <| fun (index, expected) ->
            let result = Day10.fewestTotalPresses machines[index]
            Expect.equal result expected $"Expected fewest total presses of machine[{index}] to be {expected}"
    ]
