module MachineLearning.Program

open MachineLearning.Hopfield
open MachineLearning.NeuralNetworks
open System
open System.Windows.Forms
open System.IO
open MachineLearning.Options


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

let runParametersTest pms =
    let pmsValues = match pms with 
                    | Some x -> x
                    | None -> [500.0;200.0;300.0]

    let validParameters = determineParameters 6 pmsValues
    printToFile "C:\\test\\output.txt" validParameters

let runAndDrawTSP pms = 
    let pmsValues = match pms with 
                    | Some x -> x |> Array.ofList
                    | None -> [500.0; 500.0; 200.0; 300.0] |> Array.ofList

    let parameters = paramsFromArray pmsValues
    let (cities,path) = sampleRun parameters 6
    Application.Run(drawTSP cities path)

//learns XOR, do about 2000 iteratiosn with rate 0.5
let runAndVerifyXOR iterations rate = 
    let inNetwork = createRandomNetwork 2 2 1
    let outNetwork = runTraining inNetwork iterations rate
    let out1 = (completepass [|0.0;0.0|] outNetwork).output.[0]
    let out2 = (completepass [|1.0;0.0|] outNetwork).output.[0]
    let out3 = (completepass [|0.0;1.0|] outNetwork).output.[0]
    let out4 = (completepass [|1.0;1.0|] outNetwork).output.[0]
    ()

[<EntryPoint>]
let main argv =
    Application.Run(drawAllPayOffs)
    0