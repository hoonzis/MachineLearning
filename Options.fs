module MachineLearning.Options

open System
open System.Drawing
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting
open MathNet.Numerics.Distributions
open MachineLearning.StockData

type OptionKind = 
    | Call
    | Put

type OptionLeg = 
    {
        Direction : float
        Strike : float
        Expiry : DateTime
        Kind : OptionKind
    }
    member this.TimeToExpiry = (float (this.Expiry - DateTime.Now).Days)/365.0

type CashLeg = 
    {
        Direction: float
        Strike:float
    }
    
type LegInfo = 
    | CashLeg of CashLeg
    | OptionLeg of OptionLeg

type Strategy = {
        Rate : float
        Stock : StockInfo
        Name : String
        Legs: seq<LegInfo>
    }

let normal = Normal()

let blackScholes rate (stock:StockInfo) (option:OptionLeg) =
    // We can only calculate if the option concerns the future
    if option.TimeToExpiry > 0.0 then
        // Calculate d1 and d2 and pass them to cumulative distribution
        let d1 = 
            ( log(stock.CurrentPrice / option.Strike) + 
                (rate + 0.5 * pown stock.Volatility 2) * option.TimeToExpiry ) /
            ( stock.Volatility * sqrt option.TimeToExpiry )
        let d2 = d1 - stock.Volatility * sqrt option.TimeToExpiry
        let N1 = normal.CumulativeDistribution(d1)
        let N2 = normal.CumulativeDistribution(d2)

        // Calculate the call option (and derived put option) price
        let e = option.Strike * exp (-rate * option.TimeToExpiry)
        let call = stock.CurrentPrice * N1 - e * N2
        match option.Kind with
            | Call -> call
            | Put -> call + e - stock.CurrentPrice
    else
        // If the option has expired, calculate payoff directly
        match option.Kind with
            | Call -> max (stock.CurrentPrice - option.Strike) 0.0
            | Put -> max (option.Strike - stock.CurrentPrice) 0.0

let optionPayoff option premium ref = 
    match option.Kind with
            | Call -> (max (ref - option.Strike) 0.0) - premium
            | Put -> (max (option.Strike - ref) 0.0) - premium

let cashPayoff option ref =
    (ref - option.Strike)


let getStrategyData (strategy:Strategy) = 
    let strikes = strategy.Legs |> Seq.map (fun s -> 
        match s with
            | CashLeg cl -> cl.Strike
            | OptionLeg ol -> ol.Strike
    )
    let min = 0.5*(strikes |> Seq.min)
    let max = 1.5*(strikes |> Seq.max)

    //get payoff for each leg
    let payOffs = strategy.Legs |> Seq.map (fun leg -> 
        match leg with
            | CashLeg cl -> 
                (fun ref -> cl.Direction * cashPayoff cl ref)
            | OptionLeg ol -> 
                let premium = blackScholes strategy.Rate strategy.Stock ol
                (fun ref -> ol.Direction * (optionPayoff ol premium ref))
            )

    let strategyData = [for ref in min .. max -> ref, payOffs |> Seq.sumBy (fun payOff -> payOff ref)]
    let legsData = payOffs |> Seq.map (fun payOff -> [for ref in min .. max -> ref, payOff ref])
    (strategyData, legsData)
    

let getPayoffsChart strategy = 
    let yMark = ChartTypes.TickMark(Interval = 5.0,Size = TickMarkStyle.InsideArea, Enabled = true)
    let yMinor = ChartTypes.TickMark(Interval = 1.0,Size = TickMarkStyle.InsideArea,Enabled = true)
    let xMinor = ChartTypes.TickMark(Interval = 1.0,Size = TickMarkStyle.InsideArea,Enabled = true)
    let strategyData,legsData = getStrategyData strategy

    let strategyLine = Chart.Line(strategyData,Name = strategy.Name) |> Chart.WithSeries.Style(Color = Color.Red, BorderWidth = 5)
    let legsLines = legsData |> Seq.map (fun legData -> Chart.Line(legData) |> Chart.WithSeries.Style(Color = Color.Black, BorderWidth = 2))
    let allLines = legsLines |> Seq.append [strategyLine]
    
    let chart = Chart.Combine(allLines) |> Chart.WithLegend(true) 
        //|> Chart.WithYAxis(Min = -15.0, Max = 22.0, MajorTickMark = yMark, MinorTickMark = yMinor)
        //|> Chart.WithXAxis(Min = 10.0, Max=35.0,MinorTickMark = xMinor)
    chart
