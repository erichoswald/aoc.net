namespace _2025.Solutions

open System

module Day06 =
    type Problem = {
        Width: int
        Operands: string list
        Operation: char
    }

    let parseProblems (lines: string array) =
        let mutable problems = []
        let bottom = lines.Length - 1
        let mutable right = lines[0].Length
        while right > 0 do
            right <- right - 1
            let mutable left = right
            while lines[bottom].Chars(left) = ' ' do left <- left - 1
            let problem = {
                Width = right - left + 1
                Operands = [ for row in 0 .. bottom - 1 -> lines[row][left..right] ]
                Operation = lines[bottom][left]
            }
            problems <- problem :: problems
            right <- left - 1
        problems

    let evaluateStandardNumbers (operands: string list) =
        operands |> Seq.map _.Trim() |> Seq.map Int64.Parse
    
    let evaluateCephalopodNumbers (operands: string list) =
        let length = operands[0].Length
        seq {
            for i in 1..length ->
                operands
                |> Seq.map _.Chars(length - i)
                |> Seq.filter Char.IsDigit
                |> Seq.fold (fun acc c -> acc * 10L + int64 c - int64 '0') 0L
        }

    let solveProblem (evaluate: string list -> int64 seq) (problem: Problem) =
        let operands = problem.Operands |> evaluate
        match problem.Operation with
        | '+' -> operands |> Seq.sum
        | '*' -> operands |> Seq.reduce (*)
        | _ -> failwith $"Invalid operation: '{problem.Operation}'"

    let solveStandardProblem (problem: Problem) =
        solveProblem evaluateStandardNumbers problem
    
    let solveCephalopodProblem (problem: Problem) =
        solveProblem evaluateCephalopodNumbers problem
    
    let sumStandardSolutions (problems: Problem seq) =
        problems |> Seq.map solveStandardProblem |> Seq.sum

    let sumCephalopodSolutions (problems: Problem seq) =
        problems |> Seq.map solveCephalopodProblem |> Seq.sum

    let run =
        let problems = System.IO.File.ReadAllLines "2025/Inputs/06/input.txt" |> parseProblems
        let result, millis = Measure.measure (fun () -> sumStandardSolutions problems)
        printfn $"Day 06 - Part 1: %d{result} (took: %.3f{millis}ms)"
        let result, millis = Measure.measure (fun () -> sumCephalopodSolutions problems)
        printfn $"Day 06 - Part 2: %d{result} (took: %.3f{millis}ms)"
