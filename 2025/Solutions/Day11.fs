namespace _2025.Solutions

module Day11 =
    type Device = {
        Name: string
        Outputs: string[]
    }
    type Rack = Map<string, string[]>

    let parseDevice (line: string) : Device =
        let parts = line.Split(':')
        let outputs = parts[1].Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        { Name = parts[0]; Outputs = outputs }

    let parseRack (lines: string seq) : Rack =
        lines
        |> Seq.map parseDevice
        |> Seq.map (fun device -> device.Name, device.Outputs )
        |> Map.ofSeq

    let countPaths (rack: Rack) (origin: string) (destination: string) =
        let cache = System.Collections.Generic.Dictionary<string, int64>()

        let rec countPath (name: string) =
            if not (cache.ContainsKey(name)) then
                let pathCount =
                    if name = destination then 1L
                    elif rack.ContainsKey(name) then rack[name] |> Array.sumBy countPath
                    else 0L
                cache.Add(name, pathCount)
            cache[name]
        
        countPath origin
    
    let part2 (rack: Rack) =
        let fftToDacCount = countPaths rack "fft" "dac"
        if fftToDacCount > 0 then
            let svrToFftCount = countPaths rack "svr" "fft"
            let dacToOutCount = countPaths rack "dac" "out"
            svrToFftCount * fftToDacCount * dacToOutCount
        else
            let svrToDacCount = countPaths rack "svr" "dac"
            let dacToFftCount = countPaths rack "dac" "fft"
            let fftToOutCount = countPaths rack "fft" "out"
            svrToDacCount * dacToFftCount * fftToOutCount

    let run =
        let rack = parseRack (System.IO.File.ReadLines "2025/Inputs/11/input.txt")
        let result, millis = Measure.measure (fun () -> countPaths rack "you" "out")
        printfn $"Day 11 - Part 1: %d{result} (took %.3f{millis}ms)"
        let result, millis = Measure.measure (fun () -> part2 rack)
        printfn $"Day 11 - Part 2: %d{result} (took %.3f{millis}ms)"
