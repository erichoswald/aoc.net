namespace _2025.Solutions

module Day11 =
    type Device = {
        Name: string
        Outputs: string list
    }

    let parseDevice (line: string) : Device =
        let parts = line.Split(':')
        let outputs = parts[1].Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        { Name = parts[0]; Outputs = outputs |> List.ofArray }

    let parseDevices (lines: string seq) =
        lines |> Seq.map parseDevice |> List.ofSeq

    let countPaths (devices: Device list) =
        let rack = devices |> List.map (fun d -> d.Name, d.Outputs) |> Map.ofList
        
        let rec countPath (name: string) =
            if name = "out" then
                1
            else
                rack[name] |> List.sumBy countPath
        
        countPath "you"

    let run =
        let devices = parseDevices (System.IO.File.ReadLines "2025/Inputs/11/input.txt")
        let result, millis = Measure.measure countPaths devices
        printfn $"Day 11 - Part 1: %d{result} (took %.3f{millis}ms)"
