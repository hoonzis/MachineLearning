module MachineLearning.StockData

open System
open FSharp.Data

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