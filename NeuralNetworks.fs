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