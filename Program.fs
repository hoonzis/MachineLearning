module MachineLearning.Program

open MachineLearning.Hopfield
open System
open System.Windows.Forms

[<EntryPoint>]
let main argv =
    let validParameters = determineParameters 5
    printf "%A" validParameters

    let parameters = Some ({
        A = 0.1
        B = 2.0
        D = 10.0
        u0 = 0.001
        dTime = 0.0001
    })

    
    let (cities,path) = initializeNetworkAndRun parameters 5
    Application.Run(drawTSP cities path)
    0