module MachineLearning.OptionsTests

open MachineLearning.Options

open NUnit.Framework
open FsUnit


[<Test>]
let ``test data for option call``() =
    let data = getOptionData euroCallValue 30.0 5.0 1.0
    let zeroPoint = data |> List.find (fun (i,value) -> i = 35.0)
    snd(zeroPoint) |> should equal 0