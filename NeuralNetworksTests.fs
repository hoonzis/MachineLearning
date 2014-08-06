module MachineLearning.NeuralNetworksTests

open MachineLearning.NeuralNetworks

open NUnit.Framework
open FsUnit

[<Test>]
let ``test nn hidden test 1``() =
    let weights = [
                    [(0,1);(1,1)]
                    [(1,1);(2,1)]
                  ]
    let input = [|1;1;1|]

    let actFunc x = x

    let result = applyLayer input weights actFunc
    result |> should haveLength 2
    result.[0] |> should equal 2
    result.[1] |> should equal 2

[<Test>]
let ``test nn hidden test 2``() =
    let weights = [
                    [(0,3);(1,1)]
                    [(1,1);(2,1)]
                  ]
    let input = [|2;2;1|]

    let actFunc x = x

    let result = applyLayer input weights actFunc
    result |> should haveLength 2
    result.[0] |> should equal 8
    result.[1] |> should equal 3


[<Test>]
let ``test nn hidden test 3``() =
    let weights = [|
                    [|3.0;0.0|]
                    [|1.0;1.0|]
                    [|0.0;1.0|]
                  |]
    let input = [|2.0;2.0;1.0|]

    let actFunc x = x

    let twoDimensionalWeight = Array2D.init 3 2 (fun i j -> weights.[i].[j]) 

    let result = pass input twoDimensionalWeight actFunc
    result |> should haveLength 2
    result.[0] |> should equal 8
    result.[1] |> should equal 3

[<Test>]
let ``Test xor``() =
    let network = runTraining 200 1.2
    network.output |> should equalWithin 1 0.2
