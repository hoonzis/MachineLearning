module MachineLearning.NeuralNetworks

let applyLayer (input:array<int>) (hiddenWeights:(int*int)list list) activation =
    let length = hiddenWeights |> List.length
    let output = Array.zeroCreate length
    hiddenWeights |> List.iteri (fun i weights -> 
        let sum = weights |> List.sumBy (fun (inputEl,weight) -> input.[inputEl] * weight)
        output.[i] <- activation sum
        ()
    )
    output

let deltaOutput (output:array<float>) (target:array<float>) =
    (Array.zip output target) |> Array.map (fun (o,t) -> o * (1.0 - o) * t - o)

let pass (input:float[]) (weights:float[,]) activation =
    let length = weights |> Array2D.length2
    let output = Array.zeroCreate length
    for i in 0 .. length-1 do
        let sum = (Array.zip weights.[*,i] input) |> Array.sumBy (fun (v,w) -> v * w)
        output.[i] <- activation sum
    output
        