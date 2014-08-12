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
let ``Test xor floats``() =
    xorFloats [|0.0;0.0|] |> should equal [|0.0|]
    xorFloats [|0.1;0.0|] |> should equal [|1.0|]
    xorFloats [|0.0;1.0|] |> should equal [|1.0|]
    xorFloats [|1.0;1.0|] |> should equal [|0.0|]

[<Test>]
let ``Test xor``() =
    let inNetwork = createRandomNetwork 2 2 1
    let outNetwork = runTraining inNetwork 10000 0.3
    outNetwork |> should not' (be Null)
    
    let output = completepass [|0.0;0.0|] outNetwork
    output.output.[0] |> should (equalWithin 0.2) 0.0

[<Test>]
let ``Test delta calc``() =
    let output = [|1.0;0.5;1.0|]
    let target = [|0.0;0.1;1.0|]
    let delta = deltaOutput output target
    delta |> should equal 10

[<Test>]
let ``Test pass delta``() =
    let layer = [|1.0;0.5|]
    let delta = [|1.0;0.2|]
    let weights1 = [|
                    [|3.0;0.0|]
                    [|1.0;1.0|]
                |]
    let weights = Array2D.init 2 2 (fun i j -> weights1.[i].[j])
    let passedDelta = passDelta layer delta weights
    //e1 -> 1*3+0*0.2 ->3
    //e2 -> 1*1 + 1*0.2-> 1.2
    //l1 -> 1 * (1-1)*3
    //l2 -> 0.5 * (1-0.5)*1.2 -> 0.3

    passDelta |> should equal [|1.0;0.3|]