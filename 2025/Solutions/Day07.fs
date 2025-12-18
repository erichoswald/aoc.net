namespace _2025.Solutions

module Day07 =
    let split (state: int * int Set) (row: string) =
        let _, incomingBeams = state
        let mutable splitCount, beams = state
        
        let splitBeam (col: int) =
            splitCount <- splitCount + 1
            beams <- Set.remove col beams |> Set.add (col - 1) |> Set.add (col + 1)

        let propagate (col: int) (tile: char) =
            match tile with
            | 'S' -> beams <- Set.add col beams
            | '^' when Set.contains col incomingBeams -> splitBeam col
            | _ -> ()

        row.ToCharArray() |> Array.iteri propagate
        
        splitCount, beams
    
    let splitBeams (rows: string seq) =
        rows |> Seq.fold split (0, Set.empty) |> fst

    let countBeams (counts: int64[]) (row: string) =
        let mutable nextCounts = Array.copy counts
        
        let propagate (col: int) (tile: char) =
            match tile with
            | 'S' ->
                nextCounts[col] <- 1
            | '^' when counts[col] > 0 ->
                nextCounts[col] <- 0
                nextCounts[col - 1] <- nextCounts[col - 1] + counts[col]
                nextCounts[col + 1] <- nextCounts[col + 1] + counts[col]
            | _ ->
                ()
            
        row.ToCharArray() |> Array.iteri propagate
       
        nextCounts

    let countTimelines (rows: string array) =
        rows
        |> Seq.fold countBeams (Array.create rows.Length 0L)
        |> Array.sum

    let run =
        let rows = System.IO.File.ReadAllLines("2025/Inputs/07/input.txt")
        let result, millis = Measure.measure (fun () -> splitBeams rows)
        printfn $"Day 07 - Part 1: %d{result} (took %.3f{millis}ms)"
        let result, millis = Measure.measure (fun () -> countTimelines rows)
        printfn $"Day 07 - Part 2: %d{result} (took %.3f{millis}ms)"
