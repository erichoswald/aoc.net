namespace _2025.Solutions

module Day04 =
    let Roll = '@'

    let isInGrid (grid: string array) row col =
        row >= 0 && row < grid.Length && col >= 0 && col < grid[row].Length

    let countAdjacentRolls (grid: string array) row col =
        let mutable count = 0
        for r in row-1 .. row+1 do
            for c in col-1 .. col+1 do
                if not (r = row && c = col) && isInGrid grid r c && grid[r][c] = Roll then count <- count + 1
        count

    let countAccessibleRolls (grid: string array) =
        let mutable count = 0
        for row in 0 .. grid.Length-1 do
            for col in 0 .. grid[row].Length-1 do
                if grid[row][col] = Roll then
                    let adjacentRollCount = countAdjacentRolls grid row col
                    if adjacentRollCount < 4 then
                        count <- count + 1
        count

    let parseInput file =
        System.IO.File.ReadAllLines file

    let run =
        let grid = parseInput "2025/Inputs/04/input.txt"
        printf $"Day 04 - Part 1: {countAccessibleRolls grid}"
