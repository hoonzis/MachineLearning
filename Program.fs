module MachineLearning.Program

open MachineLearning.Hopfield
open MachineLearning.NeuralNetworks
open MachineLearning.FilePrinter
open MachineLearning.StockData
open System
open System.Windows.Forms
open System.IO
open MachineLearning.Options
open MachineLearning.StrategiesExamples
open FSharp.Charting
open MathNet.Numerics.Distributions
open FSharp.Charting.ChartTypes
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting
open System.Drawing

let showChart chart = 
    let area = new ChartArea("Main")

    let control = new ChartControl(chart)
    control.Width <- 700
    control.Height  <- 500

    // Show the chart control on a top-most form
    let mainForm = new Form(Visible = true, TopMost = true, 
                            Width = 700, Height = 500)
    mainForm.Controls.Add(control)
    Application.Run mainForm

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
    showChart(getTSPChart cities path)

[<EntryPoint>]
let main argv =
    let stock = {
        Volatility = 0.5
        CurrentPrice = 100.0
    }
    let straddleExample = straddle 110.0 stock
    showChart(getPayoffsChart straddleExample)
    0