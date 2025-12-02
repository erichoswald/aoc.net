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
        
        testTheory
            "hasRepeatingPatternCount returns whether number of repeating patterns matches"
            [12341234L, 2; 123123123L, 3; 1212121212L, 5; 1111111L, 7]
             <| fun (id, count) ->
                let result = Day02.hasRepeatingPatternCount count (id.ToString())
                Expect.isTrue result (sprintf "%d should have %d repeating patterns" id count)

        testTheory "hasTwiceRepeatingPattern returns false for IDs with no or many repetitions" [1L; 23L; 101L; 333L] <| fun id ->
            let result = Day02.hasTwiceRepeatingPattern id
            Expect.isFalse result "should not contain a repeating pattern"

        testTheory "hasTwiceRepeatingPattern returns true for IDs with double patterns" [55L; 6464L; 123123L; 222222L] <| fun id ->
            let result = Day02.hasTwiceRepeatingPattern id
            Expect.isTrue result "should contain a doubly repeating pattern"
        
        testTheory "hasRepeatingPattern returns false for IDs without repeating patterns" [1L; 23L; 4567L; 123321L] <| fun id ->
            let result = Day02.hasRepeatingPattern id
            Expect.isFalse result "should not contain a repeating pattern"

        testTheory "hasRepeatingPattern returns true for IDs with repeating patterns" [11L; 1212L; 123123L; 999999L] <| fun id ->
            let result = Day02.hasRepeatingPattern id
            Expect.isTrue result "should contain a repeating pattern"

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
            let expected = 1227775554L
            Expect.equal result expected (sprintf "Part 1 should return %d for the sample input" expected)

        testCase "part2 processes sample input correctly" <| fun _ ->
            let input = System.IO.File.ReadAllText("2025/Samples/02/sample.txt").TrimEnd()
            let result = Day02.part2 input
            let expected = 4174379265L
            Expect.equal result expected (sprintf "Part 2 should return %d for the sample input" expected)
    ]
