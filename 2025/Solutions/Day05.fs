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
        
    let run =
        let ranges, ids = parseInput (System.IO.File.ReadLines("2025/Inputs/05/input.txt"))
        let result, millis = Measure.measure (fun () -> ids |> Array.filter (isFresh ranges) |> Array.length)
        printfn $"Day 05 - Part 1: %d{result} (took %.3f{millis} ms)"
