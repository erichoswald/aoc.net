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
        
        testTheory
            "parseJoltages parses joltages correctly"
            ["3,5,4,7", [|3; 5; 4; 7|]; "7,5,12,7,2", [|7; 5; 12; 7; 2|]; "10,11,11,5,10,5", [|10; 11; 11; 5; 10; 5|]]
            <| fun (input, expected) ->
                Expect.equal (Day10.parseJoltages input) expected "Expected parsed joltages"

        testCase "parsed machines contain expected values" <| fun _ ->
            Expect.equal machines.Length 3 "Expected 3 machines"
            let machine: Day10.Machine = {
                Lights = 0b0110
                Buttons = [|0b1000; 0b1010; 0b100; 0b1100; 0b101; 0b11|]
                Joltages = [|3; 5; 4; 7|]
            }
            Expect.equal machines[0] machine "Expected first machine to match sample"
        
        testTheory
            "fewestTotalPressesForLights returns expected value"
            [ 0, 2; 1, 3; 2, 2 ]
            <| fun (index, expected) ->
            let result = Day10.fewestTotalPressesForLights machines[index]
            Expect.equal result expected $"Expected fewest total presses of machine[{index}] to be {expected}"
        
        testTheory
            "patternGeneratingButtonSets returns expected value"
            [ 0, [Set([0b1000; 0b1010; 0b100]); Set([0b1000; 0b100; 0b1100; 0b101; 0b11]); Set([0b1010; 0b1100]); Set([0b11; 0b101])]]
            <| fun (index, expected) ->
                let machine = machines[index]
                let result = Day10.patternGeneratingButtonSets machine.Buttons machine.Lights
                Expect.equal result expected $"Expected pattern generation button sets of machine[{index}] to be {expected}"
        
        testCase
            "patternGeneratingButtonSets finds all button sets" <| fun _ ->
                let buttons = [|0b11000; 0b1001; 0b1010; 0b1100; 0b1111|]
                let pattern = 0b11000
                let result = Day10.patternGeneratingButtonSets buttons pattern
                let expected = [Set([|0b11000|]); Set([|0b11000; 0b1001; 0b1010; 0b1100; 0b1111|])]
                Expect.equal result expected $"Expected pattern generation button sets to be {expected}"
        
        testCase "isZero returns true when all joltages are zero" <| fun _ ->
            let result = Day10.isZero [|0; 0; 0; 0|]
            Expect.isTrue result "Expected isZero to return true"

        testCase "isZero returns false when at least one joltage is not zero" <| fun _ ->
            let result = Day10.isZero [|0; 0; 1; 0|]
            Expect.isFalse result "Expected isZero to return false"

        testCase "halveJoltages returns joltages halved 2" <| fun _ ->
                let result = Day10.halveJoltages [|3; 5; 4; 7|]
                let expected = [|1; 2; 2; 3|]
                Expect.equal result expected $"Expected %A{result} to match %A{expected}"

        testTheory
            "oddJoltagesPattern returns bit pattern of odd joltages"
            [ [|3; 5; 4; 7|], 0b1011; [|10; 11; 11; 5; 10; 5|], 0b101110 ]
            <| fun (joltages, expected) ->
                let result = Day10.oddJoltagesPattern joltages
                Expect.equal result expected $"Expected %B{result} to match %B{expected}"

        testCase "reduceJoltage decreases joltage" <| fun _ ->
            let result = Day10.reduceJoltage [|3; 5; 4; 7|] 0b1000
            Expect.equal result [|3; 5; 4; 6|] "Expected joltage to be reduced by 1"

        testTheory
            "fewestTotalPressesForJoltages returns expected value"
            [ 0, 10; 1, 12; 2, 11 ]
            <| fun (index, expected) ->
            let result = Day10.fewestTotalPressesForJoltages machines[index]
            Expect.equal result expected $"Expected fewest total presses of machine[{index}] to be {expected}"
    ]
