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
    let pmsValues = [0.00001;0.0001;0.001;0.1;0.01;1.0;5.0;10.0;50.0;100.0]
    let validParameters = determineParameters 8 pmsValues
    printToFile "C:\\test\\output.txt" validParameters
    
    //let pms =  [0.1; 0.1; 0.1; 0.1; 0.1; 1.0; 1.0] |> Array.ofList
    //let parameters = paramsFromArray pms
    //let (cities,path) = initializeNetworkAndRun parameters 8
    //Application.Run(drawTSP cities path)
    0