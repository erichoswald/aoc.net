namespace _2025.Solutions

open System.Text.RegularExpressions

module Day10 =
    type Machine = {
        Lights: int
        Buttons: int[]
    }

    let parseLights (s: string) =
        s |> Seq.rev |> Seq.fold (fun acc c -> (acc <<< 1) + if c = '#' then 1 else 0) 0

    let parseButton (s: string) =
        s.Split(',') |> Seq.fold (fun acc digits -> acc ||| (1 <<< int digits)) 0

    let parseButtons (s: string) =
        Regex.Matches(s, @"\(([\d,]+)\)")
        |> Seq.map (fun m -> parseButton (m.Groups[1].Value))
        |> Seq.toArray

    let rec parseMachine (line: string) =
        let m = Regex.Match(line, @"^\[([\.#]*)\] ([\(\d,\) ]*) {[\d,]*}$")
        { Lights = parseLights (m.Groups[1].Value); Buttons = parseButtons (m.Groups[2].Value) }

    let parseMachines (lines: string[]) =
        lines  |> Array.map parseMachine

    let fewestTotalPresses (machine: Machine) =
        let rec loop (lights: int Set) (presses: int) =
            if lights.Contains(machine.Lights) then
                presses
            else
                let lights' =
                    [
                        for button in machine.Buttons do
                            for light in lights do
                                yield light ^^^ button 
                    ] |> Set.ofSeq
                loop lights' (presses + 1)
        
        loop (Set.singleton 0) 0

    let part1 (machines: Machine[]) =
        let fewestTotalPresses = machines |> Array.sumBy fewestTotalPresses
        printfn $"Day 10 - Part 1: {fewestTotalPresses}"

    let run (part: string option) =
        let machines = System.IO.File.ReadAllLines("2025/Inputs/10/input.txt") |> parseMachines
        match part with
        | Some "1" -> part1 machines
        | None | Some _ -> part1 machines
