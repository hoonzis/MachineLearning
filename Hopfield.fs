module MachineLearning.Hopfield
open System
open MachineLearning.NeuralNetwork

type City = {
    x : float
    y : float
    i : int
}

let u0 = 0.0

let calculateDistances (cities:City list) = 
    let distances = Array2D.create 2 2 0.0
    for city in cities do
        for city1 in cities do
            let dif0 = abs(city.x-city1.x)**2.0
            let dif1 =  abs(city.y-city1.y)**2.0
            distances.[city.i,city1.i] <- sqrt (dif0 + dif1)
    distances
    

//gets tuples of row containing element and index
let rowi row (network:float[,]) = 
    network.[row,*] |> Array.mapi (fun j e -> (e,j))

let coli col (network:float[,]) = 
    network.[*,col] |> Array.mapi (fun j e -> (e,j))

let singlePass (network:float[,]) (distances) (changes:float[,]) = 
    let length = Array2D.length2 network
    for X in 0 .. length-1 do
        for i in 0 .. length-1 do
            let aSum = 2.0 * Array.fold (fun acc (e,j) -> if i<>j then acc + e else acc) 0.0 (network |> rowi X)

            let bSum = 2.0 * Array.fold (fun acc (e,Y) -> if Y<>X then acc + e else acc) 0.0 (network |> rowi i)

            let dSum = 0.0
            let dudt = -1.0*aSum - bSum - dSum
            changes.[X,i] <- dudt

    for X in 0 .. length-1 do
        for i in 0 .. length-1 do
            let changeValue = tanh(changes.[X,i]/u0)
            network.[X,i] <- 0.5 * (1.0 * changeValue)
    
    let mutable EASum = 0.0
    for X in 0 .. length-1 do
        let EARowSum = network |> rowi X |> Array.sumBy (fun (e,i) -> network.[X,i])
        EASum <- EASum + abs EARowSum

    let mutable EBSum = 0.0
    for X in 0 .. length-1 do
        let EBColSum = network |> coli X |> Array.sumBy (fun (e,i) -> network.[X,i])
        EASum <- EBSum + abs EBColSum
    EASum + EBSum