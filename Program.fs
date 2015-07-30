module MachineLearning.Program

open MachineLearning.Hopfield
open MachineLearning.NeuralNetworks
open MachineLearning.FilePrinter
open MachineLearning.StockData
open System
open System.Windows.Forms
open System.IO
open MachineLearning.Options
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


let testYahooFinanceStockData = 
    let startDate = new DateTime(2014,1,1)
    let endDate = new DateTime(2015,1,1)
    let msData = stockData "MSFT" startDate endDate
    Chart.Line msData
    

let testPriceSampling = 
    let dist1 = Normal(0.0, 1.0, RandomSource = Random(100))
    let dist2 = Normal(0.0, 1.0, RandomSource = Random(100))

    // Vary the parameters between 0.01 to 0.10
    let drift1, vol1 = 0.05, 0.10
    let drift2, vol2 = 0.20, 0.10 

    let randomPrice1 = Generator.randomPrice drift1 vol1 0.005 5.0 dist1
    let randomPrice2 = Generator.randomPrice drift2 vol2 0.005 5.0 dist2
     
    // Compare randomly generated prices 
    Chart.Combine
        [ 
            MachineLearning.Chart.SimpleLine randomPrice1 500
            MachineLearning.Chart.SimpleLine randomPrice2 500
        ]

[<EntryPoint>]
let main argv =
    showChart(testYahooFinanceStockData)
    0