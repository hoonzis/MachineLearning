module MachineLearning.HopfieldTests

open MachineLearning.Hopfield
open MachineLearning.Combinatorics
open NUnit.Framework
open FsUnit


[<Test>]
let ``Test the current path``() =
    let network = array2D [|
                    [|0.0;1.0;0.0|]
                    [|1.0;0.0;0.0|]
                    [|0.0;0.0;1.0|]
                  |]
    let path = currentPath network
    path |> should haveLength 3
    snd path.[0] |> should equal 1
    snd path.[1] |> should equal 0
    snd path.[2] |> should equal 2

[<Test>]
let ``Is Feasable correct network ``() = 
    let network = array2D [|
                    [|0.0;1.0;0.0|]
                    [|1.0;0.0;0.0|]
                    [|0.0;0.0;1.0|]
                  |]
    let path = currentPath network
    isFeasable path |> should equal true

[<Test>]
let  ``isFeasable not good path returns false``() = 
    let network = array2D [|
                    [|0.0;0.0;1.0|]
                    [|1.0;1.0;0.0|]
                    [|0.0;0.0;1.0|]
                  |]
    let path = currentPath network
    isFeasable path |> should equal false

[<Test>]
let  ``test forn``() = 
    let list = [|0.0;0.0;1.0|]
    let result = list |> forn 2 (fun i->i=0.0)
    result |>should equal true
    let result1 = list |> forn 3 (fun i->i=0.0)
    result1 |> should equal false


[<Test>]
let ``1 iteration``() =
    let cities = generateRandomCities 5
    let (network,distances,u) = initialize cities DefaultParams
    let e = singlePass network distances u DefaultParams
    network |> Array2D.length1 |> should equal 5

[<Test>]
let ``2 iterations, energy lowers``() =
    let cities = generateRandomCities 5
    let (network,distances,u) = initialize cities DefaultParams
    let e1 = singlePass network distances u DefaultParams
    let e2 = singlePass network distances u DefaultParams
    e2 |> should lessThan e1

[<Test>]
let ``10 iterations, energy lowers``() =
    let cities = generateRandomCities 4
    let (network,distances,u) = initialize cities DefaultParams
    let mutable energy = 1000.0;
    for i in 0 .. 10 do 
        let e1 = singlePass network distances u DefaultParams
        e1 |> should lessThan energy
        energy <- e1

[<Test>]
let ``100 iterations``() =
    let cities = generateRandomCities 4
    let (network,distances,u) = initialize cities DefaultParams
    for i in 0 .. 100 do 
        singlePass network distances u DefaultParams |> ignore
    let path = currentPath network
    printf "%A" path
    printf "%A" network

