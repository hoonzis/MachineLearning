// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module MachineLearning.Program

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
    
    let sigmondActivation x =
        1 / (1 + x * x)
    
    let array1 = [| 1.6; 2.3; 3.4 |]
    let array2 = [| 2.6; 4.3; 5.4 |]

    let weights = [[(1,2),(1,3)]]


    let reg = linRegressio array1 array2
    printf "regression: %A" reg
    0    