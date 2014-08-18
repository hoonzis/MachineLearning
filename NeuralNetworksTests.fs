module MachineLearning.NeuralNetworksTests

open MachineLearning.NeuralNetworks

open NUnit.Framework
open FsUnit


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
    xorFloats [|1.0;0.0|] |> should equal [|1.0|]
    xorFloats [|0.0;1.0|] |> should equal [|1.0|]
    xorFloats [|1.0;1.0|] |> should equal [|0.0|]

[<Test>]
let ``Test xor``() =
    let inNetwork = createRandomNetwork 2 2 1
    let outNetwork = runTraining inNetwork 200000 0.3
    outNetwork |> should not' (be Null)
    
    let output = completepass [|0.0;0.0|] outNetwork
    output.output.[0] |> should (equalWithin 0.1) 0.0
    
    let output2 = completepass [|1.0;0.0|] outNetwork
    output2.output.[0] |> should (equalWithin 0.1) 1.0
    
    let output3 = completepass [|0.0;1.0|] outNetwork
    output3.output.[0] |> should (equalWithin 0.1) 1.0

    let output1 = completepass [|1.0;1.0|] outNetwork
    output1.output.[0] |> should (equalWithin 0.1) 0.0

[<Test>]
let ``Test delta calc``() =
    let output = [|1.0;0.5;1.0|]
    let target = [|0.0;0.1;1.0|]
    // 1*(1-1)*(0-1) -> 0
    //0.5*(1-0.5)*(0.1 - 0.5) ->0.25*-0.4->-0.1
    let delta = deltaOutput output target
    delta |> should equal [|0.0;-0.1;0.0|]

[<Test>]
let ``Test pass delta``() =
    let layer = [|1.0;0.5|]
    let delta = [|1.0;0.2|]
    let weights = array2D [|
                    [|3.0;0.0|]
                    [|1.0;1.0|]
                |]
    let passedDelta = passDelta layer delta weights
    //e1 -> 1*3+0*0.2 ->3
    //e2 -> 1*1 + 1*0.2-> 1.2
    //l1 -> 1 * (1-1)*3
    //l2 -> 0.5 * (1-0.5)*1.2 -> 0.3

    passedDelta |> should equal [|0.0;0.3|]

[<Test>]
let ``test update weights``() =
    let layer = [|1.0;0.5|]
    let delta = [|1.0|]
    let weights = array2D [|
                    [|3.0|]
                    [|1.0|]
                |]
    let newWeights = updateWeights layer delta weights 1.0
    newWeights |> should equal (array2D [|
                                    [|4.0|]
                                    [|1.5|]
                                |])
            
