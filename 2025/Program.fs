open _2025.Solutions

[<EntryPoint>]
let main argv =
    let part = if argv.Length = 1 then None else Some argv[1]
    match argv[0] with
    | "1" -> Day01.run
    | "2" -> Day02.run
    | "3" -> Day03.run
    | "4" -> Day04.run
    | "8" -> Day08.run
    | "9" -> Day09.run
    | "10" -> Day10.run part
    | day -> printfn $"Day %s{day} not implemented yet."
    0
