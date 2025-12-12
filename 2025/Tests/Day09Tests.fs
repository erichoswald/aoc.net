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
    let coordinates = Day09.parseTiles sampleLines

    testList "Day 09 Tests" [
        testCase "parsed tiles contain expected values" <| fun _ ->
            Expect.equal coordinates.Length 8 "Expected 8 coordinates"
            Expect.equal coordinates[0] { X = 7L; Y = 1L } "Expected (7, 1) as first coordinate"
            Expect.equal coordinates[7] { X = 7L; Y = 3L } "Expected (7, 3) as last coordinate"
        
        testCase "area computes rectangular area" <| fun _ ->
            let corner: Day09.Tile = { X = 9L; Y = 7L }
            let opposite: Day09.Tile = { X = 11L; Y = 1L }
            let rect = Day09.rectangle corner opposite
            let result = Day09.area rect
            let expected = 21L
            Expect.equal result expected $"Expected area of {expected}"
        
        testCase "maximum rectangle in sample tiles has area 50" <| fun _ ->
            let result = Day09.largestRectangleArea coordinates
            let expected = 50L
            Expect.equal result expected $"Expected largest rectangle area of {expected}"
        
        testCase "largestRectangleAreaInsidePolygon in sample returns 24" <| fun _ ->
            let result = Day09.largestRectangleAreaInsidePolygon coordinates
            let expected = 24L
            Expect.equal result expected $"Expected largest area inside polygon of {expected}"
    ]