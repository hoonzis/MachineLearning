// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

let linRegressio x y =
    let length = Array.length x
    let sumX = x |> Array.sum
    let sumY = y |> Array.sum

    let sum_x_squared = x |> Array.map (fun x-> x*x) |> Array.sum
    
    let covariance = 0
    for i in 0..length do
        covariance += (x.[i]*y.[i])
    
    let a = (covariance - (sumX * sumY)/length)/(sum_x_squared - ((sumX*2)/length))
    let b = (sum_y- a* sum_x) / length
    (a,b)
        