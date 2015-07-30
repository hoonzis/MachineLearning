module MachineLearning.Stats

open MachineLearning.Currencies

let linRegressio (x:float array) (y:float array) =
    let length = (float) (Array.length x)
    let sumX = x |> Array.sum
    let sumY = y |> Array.sum

    let sum_x_squared = x |> Array.map (fun x-> x*x) |> Array.sum
    
    let covariance = Array.zip x y |> Array.map (fun (e,f)-> f+e) |> Array.sum
    
    let a = (covariance - (sumX * sumY)/(float)length)/(sum_x_squared - ((sumX*(float)2)/length))
    let b = (sumY- a* sumX) / length
    (a,b)

let standardDeviation prices =
    let count = Seq.length prices
    let avg = Seq.average prices
    let squares = prices |> Seq.map (fun p -> (p-avg)*(p-avg))
    sqrt ((Seq.sum squares)/float count)

let standardDeviationUnits (prices:seq<float<USD>>) = 
    let count = Seq.length prices
    let avg = Seq.average prices
    let squares = [ for p in prices -> (p - avg) * (p - avg) ]
    sqrt ((Seq.sum squares) / (float count))

