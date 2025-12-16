namespace _2025.Solutions

open System

module Day06 =
    type Problem = {
        numbers: int64[]
        operation: char
    }

    let parseLine (fn: string -> 'a) (line: string) =
        line.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map fn

    let parseProblems (lines: string array) =
        let numbers = lines[.. lines.Length - 2] |> Array.map (parseLine System.Int64.Parse)
        let argumentCount = numbers.Length
        let problemCount = numbers[0].Length
        let operations = lines[lines.Length - 1] |> parseLine _.Chars(0)
        Array.init problemCount (fun i ->
            { numbers = Array.init argumentCount (fun j -> numbers[j][i]); operation = operations[i] })

    let solveProblem (problem: Problem) =
        match problem.operation with
        | '+' -> problem.numbers |> Array.sum
        | '*' -> problem.numbers |> Array.reduce (*)
        | _ -> failwith $"Invalid operation: '{problem.operation}'"

    let sumSolutions (problems: Problem seq) =
        problems |> Seq.map solveProblem |> Seq.sum

    let run =
        let problems = System.IO.File.ReadAllLines "2025/Inputs/06/input.txt" |> parseProblems
        let result, millis = Measure.measure (fun () -> sumSolutions problems)
        printfn $"Day 06 - Part : %d{result} (took: %.3f{millis}ms)"
