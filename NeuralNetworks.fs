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

let passDelta (outputs:float[]) (delta:float[]) (weights:float[,]) =
    let length = weights |> Array2D.length2
    let output = Array.zeroCreate length
    for i in 0 .. length-1 do
        let error = (Array.zip weights.[*,i] delta) |> Array.sumBy (fun (v,w) -> v * w)
        output.[i] <-outputs.[i] * (1.0 - outputs.[i]) * error
    output

let updateWeights (layer:float[]) (delta:float[]) (weights:float[,]) learningRate =
    let length = weights |> Array2D.length2
    for i in 0 .. length-1 do
        weights.[*,i] |> Array.iteri (fun j x -> 
            weights.[j,i] <- learningRate * delta.[i] * layer.[j]
        )

let sigmoid x = x*x

let XOR_complete_pass input network = 
    let hidden = pass input network.inputToHidden sigmoid
    let output = pass hidden network.hiddenToOutput sigmoid
    let newNetwork = {
        inputToHidden = network.inputToHidden
        hiddenToOutput = network.hiddenToOutput
        hidden = hidden
        output = output
    }
    newNetwork
    

let train trainings network = 
    trainings |> List.iter (fun (input,target) -> 
        let n1 = XOR_complete_pass input network
        let deltaOutput = deltaOutput output target
        
    )
        