namespace _2025.Solutions

module Measure =
    let measure (fn) arg =
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let result = fn arg
        stopWatch.Stop()
        result, stopWatch.Elapsed.TotalMilliseconds
