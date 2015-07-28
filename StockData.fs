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

/// Returns stock data for a given ticker name and date range
let stockData ticker startDate endDate = 
 Stocks.Load(urlFor ticker startDate endDate)