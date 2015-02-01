module MachineLearning.Program

open MachineLearning.Hopfield
open System
open System.Windows.Forms

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    //determineParameters
    let cities = generateRandomCities 4
    let (network,distances,u) = initialize cities DefaultParams
    for i in 0 .. 100 do 
        singlePass network distances u DefaultParams |> ignore
    let path = currentPath network
    Application.Run(drawTSP cities path)
    0