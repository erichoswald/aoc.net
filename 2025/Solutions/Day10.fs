namespace _2025.Solutions

open System.Text.RegularExpressions

module Day10 =
    type Machine = {
        Lights: int
        Buttons: int[]
        Joltages: int[]
    }

    let parseLights (s: string) =
        s |> Seq.rev |> Seq.fold (fun acc c -> (acc <<< 1) + if c = '#' then 1 else 0) 0

    let parseButton (s: string) =
        s.Split(',') |> Seq.fold (fun acc digits -> acc ||| (1 <<< int digits)) 0

    let parseButtons (s: string) =
        Regex.Matches(s, @"\(([\d,]+)\)")
        |> Seq.map (fun m -> parseButton (m.Groups[1].Value))
        |> Seq.toArray

    let parseJoltages (s: string) =
        s.Split(',') |> Array.map int

    let rec parseMachine (line: string) =
        let m = Regex.Match(line, @"^\[([\.#]*)\] ([\(\d,\) ]*) {([\d,]*)}$")
        {
            Lights = parseLights (m.Groups[1].Value)
            Buttons = parseButtons (m.Groups[2].Value)
            Joltages = parseJoltages (m.Groups[3].Value)
        }

    let parseMachines (lines: string[]) =
        lines |> Array.map parseMachine

    let fewestTotalPressesForLights (machine: Machine) =
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
    
    let patternGeneratingButtonSets (buttons: int[]) (pattern: int) : int Set list =
        let rec loop (pressedBits: int) (buttonIndex: int) (buttonsUsed: int Set) (buttonSets: ResizeArray<int Set>) =
            if pressedBits = pattern then
                buttonSets.Add(buttonsUsed)
            if buttonIndex < buttons.Length then
                for i in buttonIndex .. buttons.Length - 1 do
                    loop (pressedBits ^^^ buttons[i]) (i + 1) (buttonsUsed.Add buttons[i]) buttonSets
        let buttonSets = ResizeArray()
        loop 0 0 Set.empty buttonSets
        buttonSets |> Seq.toList

    let isZero (joltages: int[]) =
        Array.forall ((=) 0) joltages
    
    let isNatural (joltages: int[]) =
        Array.forall (fun v -> v >= 0) joltages
    
    let reduceJoltage (joltage: int[]) (button: int) =
        joltage |> Array.mapi (fun i v ->
            if button &&& (1 <<< i) <> 0 then v - 1 else v)
    
    let oddJoltagesPattern (joltages: int[]) =
        joltages |> Array.fold (fun (acc, bit) v -> acc + ((v % 2) <<< bit), bit + 1) (0, 0) |> fst

    let halveJoltages (joltages: int[]) =
        joltages |> Array.map (fun v -> v >>> 1)

    let rec countPressesResultingIn (joltages: int[]) (buttons: int[]) : int =
        let cache = System.Collections.Generic.Dictionary<int[], int option>()
        
        let rec traverse (joltages: int[]) : int option =
            if cache.ContainsKey joltages then
                cache[joltages]
            elif isZero joltages then
                cache.Add(joltages, Some(0))
                Some(0) // Desired joltages have been reached.
            elif isNatural joltages then
                let oddJoltagesPattern = oddJoltagesPattern joltages
                let buttonSets = patternGeneratingButtonSets buttons oddJoltagesPattern
                let count =
                    buttonSets
                    |> List.map (fun buttonSet ->
                        let reducedJoltages = buttonSet |> Set.fold reduceJoltage joltages
                        traverse (halveJoltages reducedJoltages)
                        |> Option.map (fun v -> 2 * v + buttonSet.Count))
                    |> List.choose id
                    |> (fun counts -> if counts.IsEmpty then None else Some(counts |> List.min))
                cache.Add(joltages, count)
                count
            else
                cache.Add(joltages, None)
                None
        
        traverse joltages |> _.Value

    let fewestTotalPressesForJoltages (machine: Machine) =
        countPressesResultingIn machine.Joltages machine.Buttons

    let part1 (machines: Machine[]) =
        machines |> Array.sumBy fewestTotalPressesForLights
    
    let part2 (machines: Machine[]) =
        machines |> Array.sumBy fewestTotalPressesForJoltages

    let run =
        let machines = System.IO.File.ReadAllLines("2025/Inputs/10/input.txt") |> parseMachines
        let result, millis = Measure.measure (fun () -> part1 machines)
        printfn $"Day 10 - Part 1: %d{result} (took %.3f{millis}ms)"
        let result, millis = Measure.measure (fun () -> part2 machines)
        printfn $"Day 10 - Part 2: %d{result} (took %.3f{millis}ms)"