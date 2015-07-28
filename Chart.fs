module MachineLearning.Chart

open FSharp.Charting
open FSharp.Charting.ChartTypes

let SimpleLine seq count =
    seq |> Seq.truncate count
        |> Seq.mapi (fun i v -> i,v)
        |> Chart.Line

