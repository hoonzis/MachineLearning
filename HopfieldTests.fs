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
let  ``isFeasable not good network path returns false``() = 
    let network = array2D [|
                    [|0.0;0.0;1.0|]
                    [|1.0;1.0;0.0|]
                    [|0.0;0.0;1.0|]
                  |]
    let path = currentPath network
    isFeasable path |> should equal false

[<Test>]
let  ``isFeasable not good path returns false``() = 
    let path = [(0.0,1);(0.0,2);(0.0,1);(0.0,2)] |>Array.ofList
    isFeasable path |> should equal false

[<Test>]
let  ``test forn``() = 
    let list = [|0.0;0.0;1.0|]
    let result = list |> forn 2 (fun i->i=0.0)
    result |>should equal true
    let result1 = list |> forn 3 (fun i->i=0.0)
    result1 |> should equal false

[<Test>]
let ``distance calculation``() =
    let distances =  array2D [|
                        [|0.0;10.0;20.0|]
                        [|10.0;0.0;30.0|]
                        [|20.0;30.0;0.0|]
                      |]
    let path = [(0.0,0);(0.0,1);(0.0,2)] |>Array.ofList
    let distance = calculateDistance path distances
    distance |> should equal 40.0

[<Test>]
let ``1 iteration``() =
    let cities = generateRandomCities 5
    let distances = calculateDistances cities
    let (network,u) = initialize cities DefaultParams
    let e = singlePass network distances u DefaultParams
    network |> Array2D.length1 |> should equal 5