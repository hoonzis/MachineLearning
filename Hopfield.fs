module MachineLearning.Hopfield
open System
open MachineLearning.NeuralNetwork
open System.Drawing
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting

type City = {
    x : float
    y : float
    i : int
}

let u0 = 0.001
let u0variation = 0.001;
let dTimeInterval = 0.0001

let calculateDistances (cities:City list) = 
    let distances = Array2D.create cities.Length cities.Length 0.0
    for city in cities do
        for city1 in cities do
            let dif0 = abs(city.x-city1.x)**2.0
            let dif1 =  abs(city.y-city1.y)**2.0
            distances.[city.i,city1.i] <- sqrt (dif0 + dif1)
    distances
    

//gets tuples of row containing element and index
let rowi row (network:float[,]) = 
    network.[row,*] |> Array.mapi (fun j e -> (e,j))

let coli col (network:float[,]) = 
    network.[*,col] |> Array.mapi (fun j e -> (e,j))

let initialize (cities:City list) =
    let n = cities.Length
    let r = System.Random(System.DateTime.Now.Millisecond)
    let distances = calculateDistances cities
    let maxDistance = distances |> Seq.cast<float> |> Seq.max
    let u = Array2D.init n n (fun i j -> 
            let randomU0 = float (r.Next(100) / 100)*(u0variation*2.0)-u0variation;
            u0+randomU0
        )
    let network = Array2D.init n n (fun i j -> 0.75)
    (network,distances,u, maxDistance)


let singlePass (v:float[,]) (distances:float[,]) (u:float[,]) maxDistance = 
    let n = Array2D.length2 v
    for X in 0 .. n - 1 do
        for i in 0 .. n-1 do
            let aSum = 2.0 * Array.fold (fun acc (e,j) -> if i<>j then acc + e else acc) -1.0 (v |> rowi X)

            let bSum = 2.0 * Array.fold (fun acc (e,Y) -> if Y<>X then acc + e else acc) -1.0 (v |> rowi i)

            let mutable dSum =0.0
            for Y in 0 .. n-1 do
                let index1 = (n + 1+i) % n
                let index2  = (n+i-1%n) % n
                let dAdd = (distances.[X,Y] / maxDistance) * (v.[Y,index1] + v.[Y,index2])
                dSum <- dSum + 0.9 * dAdd

            //momentum of given node
            let dudt = -1.0*aSum - bSum - dSum
            u.[X,i] <-  u.[X,i] + dudt*dTimeInterval

    for X in 0 .. n-1 do
        for i in 0 .. n-1 do
            let ui = u.[X,i]
            let changeValue = tanh(ui/u0)
            v.[X,i] <- 0.5 * (1.0 + changeValue)
    
    let mutable EASum = 0.0
    for X in 0 .. n-1 do
        let EARowSum = v |> rowi X |> Array.fold (fun acc (e,i) -> e + acc) -1.0
        EASum <- EASum + abs EARowSum

    let mutable EBSum = 0.0
    for X in 0 .. n-1 do
        let EBColSum = v |> coli X |> Array.fold (fun acc (e,i) -> e + acc) -1.0
        EBSum <- EBSum + abs EBColSum
    EASum + EBSum

//returns the path of the current solution
let currentPath network distances =
    let n = (network |> Array2D.length2)-1
    let path = Array.zeroCreate (n+1)
    for i in 0 .. n do
        path.[i] <- (network |> coli i) |> Array.maxBy (fun (e,index) -> e)
    path

//calculates the distance of the current path
let calculateDistance (path:int[]) (distances:float[,]) =
    let mutable distance = 0
    path |> Seq.ofArray |> Seq.pairwise |> Seq.sumBy (fun (i,j) -> distances.[i,j])

let generateRandomCities n = 
    let r = System.Random()
    List.init n (fun i -> 
        { 
            i=i
            x=r.NextDouble()
            y =r.NextDouble()
        })

let drawCities (cities:City list) =
    let cityPoints = cities |> List.map (fun c -> (c.x,c.y))
    let chart = Chart.Point cityPoints
    let area = new ChartArea("Main")

    let control = new ChartControl(chart)
    control.Width <- 700
    control.Height  <- 500

    // Show the chart control on a top-most form
    let mainForm = new Form(Visible = true, TopMost = true, 
                            Width = 700, Height = 500)
    mainForm.Controls.Add(control)
    mainForm