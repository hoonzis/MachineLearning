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
let ``summ all but i-th item from row``() =
    let network = array2D [|
                    [|1.0;2.0;3.0|]
                    [|4.0;5.0;6.0|]
                    [|7.0;8.0;9.0|]
                  |]
    let result = sumAllBut 1 (network |> rowi 0)
    result |> should equal 4.0

[<Test>]
let ``summ all but i-th item from column``() =
    let network = array2D [|
                    [|1.0;2.0;3.0|]
                    [|4.0;5.0;6.0|]
                    [|7.0;8.0;9.0|]
                  |]
    let result = sumAllBut 1 (network |> coli 2)
    result |> should equal 12.0

[<Test>]
let ``dSumCalc test``() =
    let network = array2D [|
                    [|1.0;2.0;3.0|]
                    [|4.0;5.0;6.0|]
                    [|7.0;8.0;9.0|]
                  |]

    let distances =  array2D [|
                        [|0.0;10.0;20.0|]
                        [|10.0;0.0;30.0|]
                        [|20.0;30.0;0.0|]
                    |]

    //calculate the dSum for city 1 at position 2
    let result = dSumCalc distances 1 2 network
    result |> should equal 480.0

[<Test>]
let ``v Value test``() =
    let pms = paramsFromArray [|10.0;10.0;10.0;10.0|]
    let ui = 1.0/pms.alfa;
    let result = v ui pms
    result |> should equal ((1.0+tanh(1.0))/2.0)

[<Test>]
let ``singple pass test``() =
    let pms = paramsFromArray [|10.0;10.0;10.0;10.0|]
    let u = array2D [|
                    [|0.5;0.5;0.5;|]
                    [|0.5;0.5;0.5;|]
                    [|0.5;0.5;0.5;|]
                  |]

    let distances =  array2D [|
                        [|0.0;10.0;20.0|]
                        [|10.0;0.0;30.0|]
                        [|20.0;30.0;0.0|]
                    |]
    let r = singlePass distances u pms 0 1
    r |> should equal 0.49309500000000001
