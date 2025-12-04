namespace _2025.Solutions

module Day03 =
    let rec traverseAvailableBatteries batteryCount (available: int64 list) joltage : int64 =
        if batteryCount = 0 then
            // The number of requested batteries has been provided.
            joltage
        else
            // Choose the first battery with the highest joltage that leaves enough remaining batteries available.
            let index, max =
                available[.. available.Length - batteryCount]
                |> List.mapi (fun index value -> index, value)
                |> List.maxBy snd

            // Continue with the remaining batteries.
            traverseAvailableBatteries (batteryCount - 1) available[index + 1 ..] (joltage * 10L + max)

    let maximumJoltage batteryCount (bank: char seq)=
        let batteries = bank |> Seq.toList |> List.map (fun c -> int64 c - int64 '0')
        traverseAvailableBatteries batteryCount batteries 0L

    let totalMaximumJoltage batteryCount (banks: string list) =
        banks |> Seq.sumBy (maximumJoltage batteryCount)
    
    let part1 (banks: string list) = totalMaximumJoltage 2 banks
   
    let part2 (banks: string list) = totalMaximumJoltage 12 banks

    let run =
        let banks = System.IO.File.ReadAllLines "2025/Inputs/03/input.txt" |> List.ofSeq
        printfn $"Day 03 - Part 1: {part1 banks}"
        printfn $"Day 03 - Part 2: {part2 banks}"
