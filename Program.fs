module MachineLearning.Program

open MachineLearning.Hopfield
open System


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let cities = generateRandomCities 4
    let (network,distances,u, maxDistance) = initialize cities
    for i in 0 .. 10000 do
        singlePass network distances u maxDistance |> ignore
    let path = currentPath network distances
    printf "%A" path
    //Application.Run(drawCities cities)
    0