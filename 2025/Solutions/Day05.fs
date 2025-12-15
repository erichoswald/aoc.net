namespace _2025.Solutions

open System.Linq

module Day05 =
    let parseRange (line: string) =
        match line.Split('-') with
        | [| first; last |] -> (first |> System.Int64.Parse), (last |> System.Int64.Parse)
        | _ -> failwith $"Invalid pattern: {line}"
    
    let parseInput (input: string seq) =
        let ranges = input.TakeWhile(fun line -> line.Length > 0) |> Seq.map parseRange |> Seq.toArray
        let ids = input.Skip(ranges.Length + 1) |> Seq.map System.Int64.Parse |> Seq.toArray
        ranges, ids

    let isInRange range id =
        let first, last = range
        first <= id && id <= last

    let isFresh ranges id =
        ranges |> Array.exists (fun range -> isInRange range id)
        
    let mergeRanges ranges =
        let rec merge (merged: (int64 * int64) list) (range: int64 * int64) =
            let first, last = range
            match merged with
            | [] ->
                [range]
            | head :: tail when last < fst head ->
                range :: head :: tail // Insert new range before previous head.
            | head :: tail when snd head < first ->
                head :: merge tail range // Merge range after head.
            | head :: tail when last <= snd head && fst head <= first ->
                head :: tail // Ignore range as it is fully covered by the head range.
            | head :: tail when first <= fst head && snd head <= last ->
                merge tail range // Ignore previous head as range fully covers it.
            | head :: tail when first <= fst head ->
                merge tail (first, snd head) // Extend the previous head range before.
            | head :: tail when snd head <= last ->
                merge tail (fst head, last) // Extend the previous head range after.
            | head :: _ ->
                failwith $"Unexpected state: range={range} head={head}"

        ranges |> Array.fold merge []
    
    let sumRanges ranges =
        ranges |> Seq.map (fun (first, last) -> last - first + 1L) |> Seq.sum

    let run =
        let ranges, ids = parseInput (System.IO.File.ReadLines("2025/Inputs/05/input.txt"))
        let result, millis = Measure.measure (fun () -> ids |> Array.filter (isFresh ranges) |> Array.length)
        printfn $"Day 05 - Part 1: %d{result} (took %.3f{millis} ms)"
        let result, millis = Measure.measure (fun () -> mergeRanges ranges |> sumRanges)
        printfn $"Day 05 - Part 2: %d{result} (took %.3f{millis} ms)"
