module Day02Tests

open Expecto
open _2025.Solutions

[<Tests>]
let tests =
    testList "Day 02 Tests" [
        testCase "parseRange parses a single range" <| fun _ ->
            let result = Day02.parseRange "5-10" |> Seq.toList
            let expected = [5L; 6L; 7L; 8L; 9L; 10L]
            Expect.equal result expected "Range from 5 to 10 should be parsed correctly"
    
        testCase "parseInput parses a list of individual ranges" <| fun _ ->
            let input = "1-3,5-7"
            let result = Day02.parseInput input |> List.map Seq.toList
            let expected = [[1L; 2L; 3L]; [5L; 6L; 7L]]
            Expect.equal result expected "Input should be parsed into correct ranges"
        
        testTheory "isInvalidID returns false for valid IDs" [1L; 23L; 101L] <| fun id ->
            let result = Day02.isInvalidID id
            Expect.isFalse result "should be valid"

        testTheory "isInvalidID returns true for invalid ID" [55L; 6464L; 123123L] <| fun id ->
            let result = Day02.isInvalidID id
            Expect.isTrue result "should be invalid"

        testTheory
            "maxRepeatingPattern returns positive result for IDs with repeating patterns"
            [12341234L; 123123123L; 1212121212L; 1111111L]
             <| fun id ->
                let result = Day02.maxRepeatingPatternCount id
                Expect.isGreaterThan result 0 "should have repeating pattern"

        testCase "sumRepeatedTwice returns 0 for range with only valid IDs" <| fun _ ->
            let range = seq { 1698522L..1698528L }
            let result = Day02.sumRepeatedTwice range
            Expect.equal result 0 "There should be no invalid IDs in the range 1698522-1698528"

        testCase "sumRepeatedTwice returns a single invalid ID" <| fun _ ->
            let range = seq { 95L..115L }
            let result = Day02.sumRepeatedTwice range
            Expect.equal result 99L "Invalid IDs in the range 95-115 should amount to 99"

        testCase "sumRepeatedTwice sums two invalid IDs" <| fun _ ->
            let range = seq { 11L..22L }
            let result = Day02.sumRepeatedTwice range
            Expect.equal result 33L "Invalid IDs in range 11-22 should amount to 33"
        
        testTheory
            "sumRepeated sums repeating pattern IDs correctly"
            [
                seq { 11L..22L }, 33L;
                seq { 95L..115L }, 210L;
            ]
            <| fun (range, expected) ->
                let result = Day02.sumRepeated range
                Expect.equal result expected (sprintf "range %A should sum to %d" range expected)

        testCase "part1 processes sample input correctly" <| fun _ ->
            let input = System.IO.File.ReadAllText("2025/Samples/02/sample.txt").TrimEnd()
            let result = Day02.part1 input
            Expect.equal result "1227775554" "Part 1 should return 1227775554 for the sample input"

        testCase "part2 processes sample input correctly" <| fun _ ->
            let input = System.IO.File.ReadAllText("2025/Samples/02/sample.txt").TrimEnd()
            let result = Day02.part2 input
            let expected = "4174379265"
            Expect.equal result expected (sprintf "Part 2 should return %s for the sample input" expected)
    ]
