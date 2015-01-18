module MachineLearning.Program
open System.Windows.Forms
open MachineLearning.Options

let linRegressio (x:float array) (y:float array) =
    let length = (float) (Array.length x)
    let sumX = x |> Array.sum
    let sumY = y |> Array.sum

    let sum_x_squared = x |> Array.map (fun x-> x*x) |> Array.sum
    
    let covariance = Array.zip x y |> Array.map (fun (e,f)-> f+e) |> Array.sum
    
    let a = (covariance - (sumX * sumY)/(float)length)/(sum_x_squared - ((sumX*(float)2)/length))
    let b = (sumY- a* sumX) / length
    (a,b)
    
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    Application.Run(drawPayOff)
    0