open _2025.Solutions

[<EntryPoint>]
let main argv =
    match argv.[0] with
    | "1" -> Day01.run
    | "2" -> Day02.run
    | day -> printfn "Day %s not implemented yet." day
    0
