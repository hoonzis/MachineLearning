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

let longStrangle callStrike putStrike callPremium putPremium ref = 
    (euroCallValue callStrike callPremium 1.0 ref) +
    (euroPutValue putStrike putPremium 1.0 ref)

let longStraddle strike callPremium putPremium ref = longStrangle strike strike callPremium putPremium ref

let butterfly call1Strike call2Strike ref = 
    (euroCallValue call1Strike 5.0 1.0 ref) +
    (euroCallValue call2Strike 5.0 1.0 ref) -
    2.0 * (euroCallValue ((call1Strike + call2Strike) / 2.0) 5.0 1.0 ref)
    
let riskReversal strike ref =
    (euroCallValue strike 5.0 1.0 ref) +
    (euroPutValue strike 5.0 -1.0 ref)

let cashPayOff strike ref = ref - strike
    
let collar strike strike2 ref =
    (cashPayOff strike ref) +
    (euroPutValue strike 5.0 1.0 ref) +
    (euroCallValue strike2 5.0 -1.0 ref)
    
let getStrangleData callStrike putStrike = 
    [for p in 0.3*putStrike .. 2.0*callStrike -> p, (longStrangle callStrike putStrike 5.0 5.0 p)]

let getStraddleData strike = 
    [for p in floor(0.3*strike) .. floor(2.0*strike) -> p, (longStraddle strike 5.0 5.0 p)]

let getButterflyData callStrike putStrike = 
    [for p in floor(0.3*putStrike) .. floor(2.0*callStrike) -> p, (butterfly callStrike putStrike p)]

let getRiskReversalData strike = 
    [for p in floor(0.3*strike) .. floor(2.0*strike) -> p, (riskReversal strike p)]

let getCollarData strike strike2 = 
    [for p in floor(0.3*strike) .. floor(2.0*strike2) -> p, (collar strike strike2 p)]



let call1 = getOptionData euroCallValue 20.0 5.0 1.0
let call2 = getOptionData euroCallValue 22.0 5.0 1.0
let shortCalls ref = -2.0 * (euroCallValue ((18.0 + 22.0) / 2.0) 5.0 1.0 ref)
let shortCallsData = 
    [for p in 0.5*20.0 .. 2.0*20.0 -> p, (shortCalls p)]

let riskReversalBuyingCall = getOptionData euroCallValue 20.0 5.0 1.0
let riskReversalSellingPut = getOptionData euroPutValue 20.0 5.0 -1.0


let buyingStrangle = getStrangleData 22.0 20.0
let buyingStraddle = getStraddleData 20.0
let buyingButterfly = getButterflyData 22.0 18.0
let buyingRiskReversal = getRiskReversalData 20.0
let buyingCollar = getCollarData 20.0 25.0


let drawAllPayOffs = 
    printfn "%A" sellingCall

    let yMark = ChartTypes.TickMark(Interval = 5.0,Size = TickMarkStyle.InsideArea, Enabled = true)
    let yMinor = ChartTypes.TickMark(Interval = 1.0,Size = TickMarkStyle.InsideArea,Enabled = true)

    let xMinor = ChartTypes.TickMark(Interval = 1.0,Size = TickMarkStyle.InsideArea,Enabled = true)

    let chart = Chart.Combine [         
                    //Chart.Line (buyingCall, Name = "buying call") |>Chart.WithSeries.Style(Color = Color.Green, BorderWidth = 5)
                    //Chart.Line ([20.0,-30.0;20.0,30.0],Name = "strike 1")
                    Chart.Line ([20.0,-30.0;20.0,30.0],Name = "strike")
                    Chart.Line (riskReversalBuyingCall, Name = "long call") |>Chart.WithSeries.Style(Color = Color.Blue)
                    Chart.Line (riskReversalSellingPut, Name = "short put") |>Chart.WithSeries.Style(Color = Color.Red)
                    //Chart.Line (shortCallsData, Name = "2 short calls") |>Chart.WithSeries.Style(Color = Color.Green)
                    //Chart.Line(sellingPut, Name = "selling put") |>Chart.WithSeries.Style(Color = Color.Brown, BorderWidth = 5)
                    Chart.Line(buyingCollar, Name = "Collar") |> Chart.WithSeries.Style(Color = Color.Black, BorderWidth = 5)
                ]


    let withLegend = chart |> Chart.WithLegend(true) |> Chart.WithYAxis(Min = -15.0, Max = 22.0, MajorTickMark = yMark, MinorTickMark = yMinor) |> Chart.WithXAxis(Min = 10.0, Max=35.0,MinorTickMark = xMinor)

    let area = new ChartArea("Main")

    let control = new ChartControl(withLegend)
    control.Width <- 700
    control.Height  <- 500

    // Show the chart control on a top-most form
    let mainForm = new Form(Visible = true, TopMost = true, 
                            Width = 700, Height = 500)
    mainForm.Controls.Add(control)
    mainForm