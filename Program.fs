module MachineLearning.Program
open MachineLearning.NeuralNetworks
open System

let linRegressio (x:float array) (y:float array) =
    let length = (float) (Array.length x)
    let sumX = x |> Array.sum
    let sumY = y |> Array.sum

    let sum_x_squared = x |> Array.map (fun x-> x*x) |> Array.sum
    
    let covariance = Array.zip x y |> Array.map (fun (e,f)-> f+e) |> Array.sum
    
    let a = (covariance - (sumX * sumY)/(float)length)/(sum_x_squared - ((sumX*(float)2)/length))
    let b = (sumY- a* sumX) / length
    (a,b)

let determineParans =
    let inNetwork = createRandomNetwork 2 2 1
    
    printfn "%A" inNetwork

    let mutable minDelta = 1000.0
    let mutable minRate = 0.0
    let mutable minIterations = 0
    for i in 1 .. 50 do
        let iterations = i*1000
        for r in 1 .. 8 do
            let mutable totalDelta = 0.0    
            let rate = (float)r*0.1
            let network = runTraining inNetwork iterations rate
            for tIn in 0 .. (trainingPairs |> Array2D.length1)-1 do
                let input = trainingPairs.[tIn,*]
                let out = completepass input network 
                totalDelta <- totalDelta + abs((deltaOutput out.output (xorFloats input)) |> Array.sum)
            
            if minDelta > totalDelta then
                minDelta <- totalDelta
                minRate <- rate
                minIterations <- iterations
        printfn "iterations %i delta: %f" iterations minDelta
    printfn "best params: %i - %f - %f" minIterations minRate minDelta

    
[<EntryPoint>]
let main argv = 
    let array1 = [| 1.6; 2.3; 3.4 |]
    let array2 = [| 2.6; 4.3; 5.4 |]

    let reg = linRegressio array1 array2
    printf "regression: %A" reg

    determineParans
    Console.ReadKey(true)
    0    