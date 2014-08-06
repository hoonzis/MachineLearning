module MachineLearning.NeuralNetworks
open System

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
    weights



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

let train network rate input target =
    let n1 = XOR_complete_pass input network
    let delta = deltaOutput n1.output target
    let deltaHidden = passDelta n1.output delta n1.hiddenToOutput
    let updatedHiddenToOut = updateWeights n1.output delta n1.hiddenToOutput rate
    let updatedInToHidden = updateWeights n1.hidden deltaHidden n1.inputToHidden rate
    {
        inputToHidden = updatedInToHidden
        hiddenToOutput = updatedHiddenToOut
        hidden = Array.empty
        output = Array.empty
    }

    
let inline xor a b = (a || b) && not (a && b)

let runTraining iterations rate =
    let xorFloats input:float[] =
        let a:bool = Convert.ToBoolean(input.[0])
        let b:bool = Convert.ToBoolean(input.[1])
        a xor b

    let rec reduce trainings network = 
        match trainings with
            | (a,b) :: tail -> 
                let n1 = train network rate [|a;b|] (xorFloats [|a;b|])
                reduce tail n1
            | [] -> network

    let pairs = [
                    (0,0)
                    (0,1)
                    (1,0)
                    (1,1)
                ] |> Array.ofList
    let allTrainings = [| for i in 0 .. iterations -> pairs.[i%4]|]

    let weights = [|
        [|3.0;0.0|]
        [|1.0;1.0|]
        [|0.0;1.0|]
    |]
    
    let exampleWeights = Array2D.init 3 2 (fun i j -> weights.[i].[j]) 

    let startNetwork = {
        inputToHidden = exampleWeights
        hiddenToOutput = exampleWeights
        hidden = Array.empty
        output = Array.empty
    }

    let result = reduce allTrainings startNetwork xorFloats
    result