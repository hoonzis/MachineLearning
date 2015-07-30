module MachineLearning.StockData

open System
open System.Collections.Generic
open FSharp.Data
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions

type Stocks = CsvProvider<"testdata.csv">

/// Helper function that returns URL for required stock data
let urlFor ticker (startDate:System.DateTime) (endDate:System.DateTime) = 
    let root = "http://ichart.finance.yahoo.com"
    sprintf "%s/table.csv?s=%s&a=%i&b=%i&c=%i&d=%i&e=%i&f=%i" 
        root ticker (startDate.Month - 1) startDate.Day startDate.Year 
                    (endDate.Month - 1) endDate.Day endDate.Year

let stockData ticker startDate endDate = 
    let csvData = Stocks.Load(urlFor ticker startDate endDate)
    csvData.Rows

//return date close paris for given stock
let stockDataSimplified ticker startDate endDate = 
    let csvData = Stocks.Load(urlFor ticker startDate endDate)
    csvData.Rows |> Seq.map (fun item -> item.Date,item.Close)

let logRatios ticker startDate endDate =
    let data =  stockData ticker startDate endDate
    data |> Seq.sortBy (fun a -> a.Date)
         |> Seq.pairwise
         |> Seq.map (fun (prev,next) -> log (float next.Close / float prev.Close))

//this holds
//mean = (drift - 0.5 * volatility2) * tau
//variance = volatility2 * tau
let getVolatilityAndDrift (logRatios:IEnumerable<float>) = 
    let stats = DescriptiveStatistics(logRatios,false)
    let tau = 1.0 / 252.0

    let volatility = sqrt (stats.Variance / tau)
    let drift = (stats.Mean / tau) + (pown volatility 2) / 2.0
    (volatility,drift)

let dist1 = Normal(0.0, 1.0, RandomSource = Random(100))
let dist2 = Normal(0.0, 2.0, RandomSource = Random(100))

let rec randomWalk value (dist:IContinuousDistribution) = seq {
    yield value
    yield! randomWalk (value + dist.Sample()) dist
}

let randomPrice drift volatility dt initial (dist:Normal) = 
    // Calculate parameters of the exponential
    let driftExp = (drift - 0.5 * pown volatility 2) * dt
    let randExp = volatility * (sqrt dt)

    // Recursive loop that actually generates the price
    let rec loop price = seq {
        yield price
        let price = price * exp (driftExp + randExp * dist.Sample()) 
        yield! loop price }

    // Return path starting at 'initial'
    loop initial

let simulatePrice ticker startDate endDate =
    let historicalData = logRatios ticker startDate endDate
    let (vol, drift) = historicalData |> getVolatilityAndDrift
    