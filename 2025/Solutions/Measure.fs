namespace _2025.Solutions

module Measure =
    let measure (computation: unit -> 'a) =
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let result = computation ()
        stopWatch.Stop()
        result, stopWatch.Elapsed.TotalMilliseconds
