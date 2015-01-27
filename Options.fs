module MachineLearning.Options

open System
open System.Drawing
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting


let euroCallValue strike premium buySell ref = 
    buySell * ((max (ref - strike) 0.0) - premium)

let euroPutValue strike premium buySell ref =
    buySell * ((max (strike - ref) 0.0) - premium)

let optionPayOff option = 
    [for p in 0.0 .. 10.0 .. 80.0 -> p, option p]

let getOptionData option strike premium buySell =
    [for p in 0.5*strike .. 2.0*strike -> p, (option strike premium buySell p)]

let buyingCall = getOptionData euroCallValue 30.0 5.0 1.0

let sellingCall = getOptionData euroCallValue 30.0 5.0 -1.0
    
let buyingPut = getOptionData euroPutValue 30.0 5.0 1.0

let sellingPut = getOptionData euroPutValue 30.0 5.0 -1.0

let drawPayOff = 

    printfn "%A" sellingCall
    let chart = Chart.Combine [         
                    Chart.Line (buyingCall, Name = "buying call")
                    Chart.Line (sellingCall, Name = "selling call")
                    Chart.Line (buyingPut, Name = "buying put")
                    Chart.Line(sellingPut, Name = "selling put")
                ]

    let withLegend = chart |> Chart.WithLegend(true)
    let area = new ChartArea("Main")

    let control = new ChartControl(withLegend)
    control.Width <- 700
    control.Height  <- 500

    // Show the chart control on a top-most form
    let mainForm = new Form(Visible = true, TopMost = true, 
                            Width = 700, Height = 500)
    mainForm.Controls.Add(control)
    mainForm