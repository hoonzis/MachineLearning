module MachineLearning.NeuralNetworks
open System
open MachineLearning.NeuralNetwork

let trainingPairs =  array2D [|
                        [|0.0;0.0|]
                        [|0.0;1.0|]
                        [|1.0;0.0|]
                        [|1.0;1.0|]
                    |]

let deltaOutput (output:array<float>) (target:array<float>) =
    (Array.zip output target) |> Array.map (fun (o,t) -> o * (1.0 - o) * (t - o))

let pass (input:float[]) (weights:float[,]) activation =
    let length = weights |> Array2D.length2
    seq {
        for i in 0 .. length-1 do
            let sum = (Array.zip weights.[*,i] input) |> Array.sumBy (fun (v,w) -> v * w)
            yield activation sum
    } |> Array.ofSeq

let passDelta (outputs:float[]) (delta:float[]) (weights:float[,]) =
    let length = weights |> Array2D.length1
    let output = Array.zeroCreate length
    for i in 0 .. length-1 do
        let error = (Array.zip weights.[i,*] delta) |> Array.sumBy (fun (v,w) -> v * w)
        output.[i] <-outputs.[i] * (1.0 - outputs.[i]) * error
    output



let updateWeights (layer:float[]) (delta:float[]) (weights:float[,]) learningRate =
    let outputCount = weights |> Array2D.length2
    let hiddenCount = weights |> Array2D.length1

    for i in 0 .. outputCount-1 do 
        for j in 0 .. hiddenCount-1 do
            weights.[j,i] <- weights.[j,i] + learningRate * delta.[i] * layer.[j]
    weights


let sigmoid value = 1.0/(1.0 + exp(-value));

let completepass input network = 
    let hidden = pass input network.inputToHidden sigmoid
    let output = pass hidden network.hiddenToOutput sigmoid
    let newNetwork = {
        input = input
        inputToHidden = network.inputToHidden
        hiddenToOutput = network.hiddenToOutput
        hidden = hidden
        output = output
    }
    newNetwork

let train network rate input target =
    let n1 = completepass input network
    let delta = deltaOutput n1.output target
    let deltaHidden = passDelta n1.hidden delta n1.hiddenToOutput
    let updatedHiddenToOut = updateWeights n1.hidden delta n1.hiddenToOutput rate
    let updatedInToHidden = updateWeights n1.input deltaHidden n1.inputToHidden rate
    {
        input = input
        inputToHidden = updatedInToHidden
        hiddenToOutput = updatedHiddenToOut
        hidden = Array.empty
        output = Array.empty
    }

let xorFloats = function
        | [|0.0;0.0|] -> [|0.0|]
        | [|1.0;0.0|] -> [|1.0|]
        | [|0.0;1.0|] -> [|1.0|]
        | [|1.0;1.0|] -> [|0.0|]
        | _ -> failwith "This XOR is only for 2 inputs" 

let createRandomNetwork inputNodes hiddenNodes outputNodes = 
    let rnd = System.Random()
    let inputToHidden = Array2D.init inputNodes hiddenNodes (fun _ _ -> rnd.NextDouble() + 0.3)
    let hiddenToOutput = Array2D.init hiddenNodes outputNodes (fun _ _ -> rnd.NextDouble() + 0.3)
    let network = {
        input =  Array.empty
        inputToHidden = inputToHidden
        hiddenToOutput = hiddenToOutput
        hidden = Array.empty
        output = Array.empty
    }
    network
    
let runTraining network iterations rate =
    let rec reduce trainings network = 
        match trainings with
            | input :: tail -> 
                let n1 = train network rate input (xorFloats input)
                reduce tail n1
            | [] -> network


    let allTrainings = [for i in 0 .. iterations -> trainingPairs.[i%4,*]]

    let result = reduce allTrainings network
    result