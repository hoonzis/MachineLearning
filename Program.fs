module MachineLearning.Program

open MachineLearning.Hopfield
open System
open System.Windows.Forms

[<EntryPoint>]
let main argv =
    //let validParameters = determineParameters 6
    //printf "%A" validParameters

    let parameters = Some ({
        A = 0.1
        B = 0.1
        D = 2.0
        u0 = 30.0
        dTime = 0.0001
    })

    
    let (cities,path) = initializeNetworkAndRun parameters 6
    Application.Run(drawTSP cities path)
    0