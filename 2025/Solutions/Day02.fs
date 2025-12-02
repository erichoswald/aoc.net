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

    let isInvalidID (id:int64) : bool =
        let digits = id.ToString()
        if digits.Length % 2 = 1 then
            false
        else
            let halfLength = digits.Length / 2
            digits[..(halfLength - 1)] = digits[halfLength..]

    let maxRepeatingPatternCount (id:int64) : int =
        let digits = id.ToString()
        let length = digits.Length
        let rec repeatingPatternCount size =
            if size > length / 2 then
                0
            else
                let pattern = digits[..(size - 1)]
                let count = length / size
                if String.replicate count pattern = digits then
                    count
                else
                    repeatingPatternCount (size + 1)
        repeatingPatternCount 1

    let hasRepeatingPattern (id:int64) : bool =
        maxRepeatingPatternCount id > 1

    let sumInvalidIDs (isInvalidID: int64 -> bool) (range : int64 seq) : int64 =
        range
        |> Seq.filter isInvalidID
        |> Seq.sum

    let sumRepeatedTwice (range : int64 seq) : int64 =
        sumInvalidIDs isInvalidID range

    let sumRepeated (range : int64 seq) : int64 =
        sumInvalidIDs hasRepeatingPattern range

    let part1 (input:string) : string =
        parseInput input
        |> List.map sumRepeatedTwice
        |> List.sum
        |> string

    let part2 (input:string) : string =
        parseInput input
        |> List.map sumRepeated
        |> List.sum
        |> string

    let run =
        let input = System.IO.File.ReadAllText("2025/Inputs/02/input.txt").TrimEnd()
        printfn "Day 02 - Part 1: %s" (part1 input)
        printfn "Day 02 - Part 2: %s" (part2 input)
