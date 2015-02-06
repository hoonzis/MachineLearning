module MachineLearning.Program

open MachineLearning.Hopfield
open System
open System.Windows.Forms
open System.IO

let printToFile filename obj =
    let myDocsPath = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) 
    let fullPath = Path.Combine(myDocsPath, filename)

    let sb = new System.Text.StringBuilder()
    let myPrint format = Printf.bprintf sb format 
    
    use rw = new StreamReader(path=fullPath)
    let old = rw.ReadToEnd()
    rw.Close()
    do myPrint "%s" old
    do myPrint "%A" obj

    use sw = new StreamWriter(path=fullPath)
    // use partial application to fix the TextWriter
    let myPrintF format = fprintf sw format
    
    let toWrite = sb.ToString()
    do myPrintF "%s" toWrite

    //get the result
    sw.Close()

[<EntryPoint>]
let main argv =
    let validParameters = determineParameters 6
    printToFile "C:\\test\\output.txt" validParameters

    let parameters = Some ({
        A = 0.1
        B = 0.1
        D = 2.0
        u0 = 30.0
        dTime = DefaultParams.C
        Rho = DefaultParams.C
        C = DefaultParams.C
    })

    
    let (cities,path) = initializeNetworkAndRun parameters 6
    Application.Run(drawTSP cities path)
    0