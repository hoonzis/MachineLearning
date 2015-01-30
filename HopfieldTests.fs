module MachineLearning.HopfieldTests

open MachineLearning.Hopfield

open NUnit.Framework
open FsUnit


[<Test>]
let ``test 1 iteration``() =
    let cities = generateRandomCities 5
    let (network,distances,u, maxDistance) = initialize cities
    let e = singlePass network distances u maxDistance
    network |> Array2D.length1 |> should equal 5

[<Test>]
let ``test 2 iterations, energy lowers``() =
    let cities = generateRandomCities 5
    let (network,distances,u, maxDistance) = initialize cities
    let e1 = singlePass network distances u maxDistance
    let e2 = singlePass network distances u maxDistance
    e2 |> should lessThan e1