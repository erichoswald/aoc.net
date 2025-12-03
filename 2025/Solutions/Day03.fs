namespace _2025.Solutions

module Day03 =
    let rec weakestIndex selection index next =
        match selection with
        | [] -> None // Not expected except in test.
        | [ last ] when last < next -> Some index // Replace last value.
        | a :: b :: _ when a < b -> Some index // Replace the first value whose successor is higher in value.
        | _ :: tail -> weakestIndex tail (index + 1) next // Keep looking.

    let maximize selection battery =
        match weakestIndex selection 0 battery with
        | None -> selection
        | Some i -> selection[0 .. i - 1] @ selection[i + 1 ..] @ [ battery ]

    let joltage selection =
        selection |> List.fold (fun acc digit -> 10L * acc + digit) 0L

    let largestJoltage n bank =
        bank
        |> Seq.map (fun c -> int64 c - int64 '0')
        |> Seq.fold maximize (List.replicate n 0L)
        |> joltage

    let part1 (banks: string list) = banks |> Seq.sumBy (largestJoltage 2)
   
    let part2 (banks: string list) = banks |> Seq.sumBy (largestJoltage 12)

    let run =
        let input = System.IO.File.ReadAllLines "2025/Inputs/03/input.txt" |> List.ofSeq
        printfn $"Day 03 - Part 1: {part1 input}"
        printfn $"Day 03 - Part 2: {part2 input}"
