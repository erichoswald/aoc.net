namespace _2025.Solutions

module Day04 =
    let parseInput file =
        System.IO.File.ReadAllLines file
        |> Array.map _.ToCharArray()
        |> array2D

    let isInGrid (grid: char array2d) row col =
        row >= 0 && row < Array2D.length1 grid && col >= 0 && col < Array2D.length2 grid
    
    let isRoll (grid: char array2d) row col =
        grid[row, col] = '@'

    let coords top bottom left right = [
        for row in top .. bottom do
            for col in left .. right do
                yield row, col
    ]

    let countAdjacentRolls (grid: char array2d) row col =
        let mutable count = 0
        for r, c in coords (row - 1) (row + 1) (col - 1) (col + 1) do
            if not (r = row && c = col) && isInGrid grid r c && isRoll grid r c then count <- count + 1
        count

    let isAccessibleRoll (grid: char array2d) row col =
        countAdjacentRolls grid row col < 4

    let listAccessibleRolls (grid: char array2d) =
        let rows = Array2D.length1 grid
        let cols = Array2D.length2 grid
        let mutable rolls = []
        for row, col in coords 0 (rows - 1) 0 (cols - 1) do
            if isRoll grid row col && isAccessibleRoll grid row col then
                rolls <- (row, col) :: rolls
        rolls

    let countAccessibleRolls (grid: char array2d) =
        (listAccessibleRolls grid).Length

    let removeRolls (grid: char array2d) (rolls: (int * int) list) =
        for row, col in rolls do
            grid[row, col] <- '.'

    let removeAllAccessibleRolls (grid: char array2d) =
        let mutable count = 0
        let mutable accessibleRolls = listAccessibleRolls grid
        while accessibleRolls.Length > 0 do
            count <- count + accessibleRolls.Length
            removeRolls grid accessibleRolls
            accessibleRolls <- listAccessibleRolls grid
        count

    let run =
        let grid = parseInput "2025/Inputs/04/input.txt"
        printfn $"Day 04 - Part 1: {countAccessibleRolls grid}"
        printfn $"Day 04 - Part 2: {removeAllAccessibleRolls grid}"
