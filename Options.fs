module MachineLearning.Options

open System
open System.Drawing
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting
open MathNet.Numerics.Distributions
open MachineLearning.StockData

type OptionType = 
    | Call
    | Put

type OptionInfo = 
    {
        Strike : float
        Expiry : DateTime
        Kind : OptionType
    }
    member this.TimeToExpiry = (float (this.Expiry - DateTime.Now).Days)/365.0

type Strategy = {
        Stock : StockInfo
        Name : String
        Legs: seq<OptionInfo>
    }

// Used to get the cumulative distribution function
let normal = Normal()

/// Calculates the price of 'option' for a given 'stock' and 
/// a global interest 'rate' using the Black-Scholes equation
let blackScholes rate (stock:StockInfo) (option:OptionInfo) =
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

let optionValue option stock rate = 
    let premium = blackScholes rate stock option
    match option.Kind with
            | OptionType.Call -> (max (stock.CurrentPrice - option.Strike) 0.0) - premium
            | OptionType.Put -> (max (stock.CurrentPrice - option.Strike) 0.0) - premium


let optionPayOff option = 
    [for p in 0.0 .. 10.0 .. 80.0 -> p, option p]

let getOptionData option strike premium buySell =
    [for p in 0.5*strike .. 2.0*strike -> p, (option strike premium buySell p)]


let euroCallValue strike ref = 
    let stock = {
        CurrentPrice = ref
        Volatility = 10.0
    }

    let option = {
        Kind = OptionType.Call
        Strike = strike
        Expiry = DateTime.Now.AddMonths(6)
    }
    let value = optionValue option stock ref
    value

let euroPutValue strike ref = 
    let stock = {
        CurrentPrice = ref
        Volatility = 10.0
    }

    let option = {
        Kind = OptionType.Call
        Strike = strike
        Expiry = DateTime.Now.AddMonths(6)
    }
    optionValue option stock ref    

let longStrangle callStrike putStrike callPremium putPremium ref = 
    (euroCallValue callStrike ref) +
    (euroPutValue putStrike ref)

let longStraddle strike callPremium putPremium ref = longStrangle strike strike callPremium putPremium ref

let butterfly call1Strike call2Strike ref = 
    (euroCallValue call1Strike ref) +
    (euroCallValue call2Strike ref) -
    2.0 * (euroCallValue ((call1Strike + call2Strike) / 2.0) ref)

let riskReversal strike ref =
    (euroCallValue strike ref) -
    (euroPutValue strike ref)

let cashPayOff strike ref = ref - strike
    
let collar strike strike2 ref =
    (cashPayOff strike ref) +
    (euroPutValue strike ref) -
    (euroCallValue strike2 ref)

let condor ref = 
    - (euroCallValue 14.0 ref) +
    - (euroCallValue 12.0 ref) +
    - (euroCallValue 20.0 ref) +
    (euroCallValue 22.0 ref)
    //selling 1 in the money call
    //buy 1 in the money call (lower strike)
    //sell 1 out of money call
    //buy 1 out of money call (higher strike)

let boxOption strike1 strike2 ref = 
    (euroCallValue strike1 ref) +
    - (euroCallValue strike2 ref) +
    (euroCallValue strike2 ref) +
    - (euroCallValue strike1 ref)

    
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

let getCondorData currentRef =
    [for p in floor(0.3*currentRef) .. floor(2.0*currentRef) -> p, (condor p)]

let getBoxData strike strike2 = 
    [for p in floor(0.3*strike) .. floor(2.0*strike2) -> p, (boxOption strike strike2 p)]
    
let shortCalls ref = -2.0 * (euroCallValue ((18.0 + 22.0) / 2.0) ref)
let shortCallsData = 
    [for p in 0.5*20.0 .. 2.0*20.0 -> p, (shortCalls p)]

let buyingStrangle = getStrangleData 22.0 20.0
let buyingStraddle = getStraddleData 20.0
let buyingButterfly = getButterflyData 22.0 18.0
let buyingRiskReversal = getRiskReversalData 20.0
let buyingCollar = getCollarData 20.0 25.0

let getPayoffsChart = 

    let yMark = ChartTypes.TickMark(Interval = 5.0,Size = TickMarkStyle.InsideArea, Enabled = true)
    let yMinor = ChartTypes.TickMark(Interval = 1.0,Size = TickMarkStyle.InsideArea,Enabled = true)
    let xMinor = ChartTypes.TickMark(Interval = 1.0,Size = TickMarkStyle.InsideArea,Enabled = true)


    let chart = Chart.Combine [         
                    Chart.Line((getCondorData 40.0), Name = "Condor") |> Chart.WithSeries.Style(Color = Color.Black, BorderWidth = 5)
                ]


    chart |> Chart.WithLegend(true) |> Chart.WithYAxis(Min = -15.0, Max = 22.0, MajorTickMark = yMark, MinorTickMark = yMinor) |> Chart.WithXAxis(Min = 10.0, Max=35.0,MinorTickMark = xMinor)
