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