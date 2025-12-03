namespace _2025.Solutions

module Day03 =
    let largestJoltage (bank: string) =
        bank
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.fold
            (fun (leadingDigit, maxJoltage) battery ->
                let joltage = 10 * leadingDigit + battery
                if battery > leadingDigit then
                    battery, max maxJoltage joltage
                else
                    leadingDigit, max maxJoltage joltage)
            (0, 0)
        |> snd

    let part1 (banks: string list) = banks |> Seq.sumBy largestJoltage

    let run =
        let input = System.IO.File.ReadAllLines "2025/Inputs/03/input.txt" |> List.ofSeq
        printfn $"Day 03 - Part 1: {part1 input}"
