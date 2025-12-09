module Day09Tests

open Expecto
open _2025.Solutions

let sampleLines = [|
    "7,1"
    "11,1"
    "11,7"
    "9,7"
    "9,5"
    "2,5"
    "2,3"
    "7,3"
|]

[<Tests>]
let tests =
    let coordinates = Day09.parseCoordinates sampleLines

    testList "Day 09 Tests" [
        testCase "parsed coordinates contain expected values" <| fun _ ->
            Expect.equal coordinates.Length 8 "Expected 8 coordinates"
            Expect.equal coordinates[0] (7L, 1L) "Expected (7, 1) as first coordinate"
            Expect.equal coordinates[7] (7L, 3L) "Expected (7, 3) as last coordinate"
        
        testCase "area computes rectangular area" <| fun _ ->
            let corner = 9L,7L
            let opposite = 11L,1L
            let result = Day09.area corner opposite
            let expected = 21L
            Expect.equal result expected $"Expected area of {expected}"
        
        testCase "maximum rectangle in sample coordinates has area 50" <| fun _ ->
            let result = Day09.largestRectangleArea coordinates
            let expected = 50L
            Expect.equal result expected $"Expected largest rectangle area of {expected}"
    ]