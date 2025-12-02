namespace _2025.Solutions

open System

module Day02 =
    let parseRange (range:string) : seq<int64> =
        let parts = range.Split('-')
        let first = Int64.Parse(parts.[0])
        let last = Int64.Parse(parts.[1])
        seq { first..last }

    let parseInput (input:string) : int64 seq list =
        input.Split(',')
        |> Array.toList
        |> List.map parseRange

    let hasRepeatingPatternCount count (digits:string) : bool =
        let pattern = digits[..(digits.Length / count - 1)]
        String.replicate count pattern = digits

    let hasTwiceRepeatingPattern id : bool =
        hasRepeatingPatternCount 2 (id.ToString())

    let hasRepeatingPattern id : bool =
        let digits = id.ToString()
        let rec hasRepeatingPattern count =
            if hasRepeatingPatternCount count digits then true
            else if count < digits.Length then hasRepeatingPattern (count + 1)
            else false
        hasRepeatingPattern 2

    let sumMatchingIDs (predicate: int64 -> bool) (range : int64 seq) : int64 =
        range
        |> Seq.filter predicate
        |> Seq.sum

    let sumRepeatedTwice (range : int64 seq) : int64 =
        sumMatchingIDs hasTwiceRepeatingPattern range

    let sumRepeated (range : int64 seq) : int64 =
        sumMatchingIDs hasRepeatingPattern range

    let part (sumMatches: int64 seq -> int64) (input:string) : int64 =
        parseInput input
        |> List.map sumMatches
        |> List.sum

    let part1 (input:string) : int64 =
        part sumRepeatedTwice input

    let part2 (input:string) : int64 =
        part sumRepeated input

    let run =
        let input = System.IO.File.ReadAllText("2025/Inputs/02/input.txt").TrimEnd()
        printfn "Day 02 - Part 1: %d" (part1 input)
        printfn "Day 02 - Part 2: %d" (part2 input)
