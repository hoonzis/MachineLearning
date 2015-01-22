module MachineLearning.Hopfield
open System
open MachineLearning.NeuralNetwork


let singlePass (network:float[,]) = 
    let length = Array2D.length2 network distances
    for X in 0 .. length-1 do
        for i in 0 .. length-1 do
            let xValues = network.[X,*] |> Array.mapi (fun j e -> (e,j))
            let aSum = 2.0 * Array.fold (fun acc (e,j) -> if i<>j then acc + e else acc) 0.0 xValues

            let iValues = network.[i,*] |> Array.mapi (fun Y e -> (e,Y))
            let bSum = 2.0 * Array.fold (fun acc (e,Y) -> if Y<>X then acc + e else acc) 0.0 xValues |> ignore
    0
            
  