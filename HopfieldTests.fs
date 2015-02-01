module MachineLearning.HopfieldTests

open MachineLearning.Hopfield

open NUnit.Framework
open FsUnit


[<Test>]
let ``1 iteration``() =
    let cities = generateRandomCities 5
    let (network,distances,u, maxDistance) = initialize cities
    let e = singlePass network distances u maxDistance
    network |> Array2D.length1 |> should equal 5

[<Test>]
let ``2 iterations, energy lowers``() =
    let cities = generateRandomCities 5
    let (network,distances,u, maxDistance) = initialize cities
    let e1 = singlePass network distances u maxDistance
    let e2 = singlePass network distances u maxDistance
    e2 |> should lessThan e1

[<Test>]
let ``10 iterations, energy lowers``() =
    let cities = generateRandomCities 4
    let (network,distances,u, maxDistance) = initialize cities
    let mutable energy = 1000.0;
    for i in 0 .. 10 do 
        let e1 = singlePass network distances u maxDistance
        e1 |> should lessThan energy
        energy <- e1

[<Test>]
let ``100 iterations``() =
    let cities = generateRandomCities 4
    let (network,distances,u, maxDistance) = initialize cities
    let mutable energy = 1000.0;
    for i in 0 .. 100 do 
        singlePass network distances u maxDistance |> ignore
    let path = currentPath network distances
    printf "%A" path
    printf "%A" network