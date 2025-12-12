namespace _2025.Solutions

module Day09 =
    [<Struct>]
    type Tile = { X: int64; Y: int64 }
    
    [<Struct>]
    type Rectangle = { Left: int64; Right: int64; Top: int64; Bottom: int64 }
    
    let parseTile (line: string) : Tile =
        let coordinates = line.Split(',') |> Array.map System.Int64.Parse
        { X = coordinates[0]; Y = coordinates[1] }
    
    let parseTiles (lines: string[]) =
        lines |> Array.map parseTile

    let rectangle (u: Tile) (v: Tile) =
        { Left = min u.X v.X; Right = max u.X v.X; Top = max u.Y v.Y; Bottom = min u.Y v.Y }

    let area (r: Rectangle) =
        let width = r.Right - r.Left + 1L
        let height = r.Top - r.Bottom + 1L
        width * height

    let rectangles (tiles: Tile seq) =
        seq {
            for u in tiles do
                for v in tiles do
                    if u <> v then
                        yield rectangle u v
        }

    let largestRectangleArea (tiles: Tile seq) =
        rectangles tiles |> Seq.maxBy area |> area

    let intersect (r: Rectangle) (s: Rectangle) =
        r.Left < s.Right && r.Right > s.Left && r.Bottom < s.Top && r.Top > s.Bottom

    let largestRectangleAreaInsidePolygon (tiles: Tile[]) =
        let edges = [
            for i = 0 to tiles.Length - 1 do
                yield rectangle tiles[i] tiles[(i + 1) % tiles.Length]
        ]
        rectangles tiles
        |> Seq.sortByDescending area
        |> Seq.filter (fun r -> edges |> Seq.exists (intersect r) |> not)
        |> Seq.head
        |> area

    let run =
        let input = System.IO.File.ReadAllLines "2025/Inputs/09/input.txt"
        let tiles = parseTiles input
        let result, millis = Measure.measure (fun () -> largestRectangleArea tiles)
        printfn $"Day 09 - Part 1: %d{result} (took {millis}ms)"
        let result, millis = Measure.measure (fun () -> largestRectangleAreaInsidePolygon tiles)
        printfn $"Day 09 - Part 2: %d{result} (took {millis}ms"