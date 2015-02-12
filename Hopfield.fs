module MachineLearning.Hopfield

open System
open MachineLearning.NeuralNetwork
open MachineLearning.Combinatorics
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

type HopfieldTspParams = {
    A: float
    B: float
    D: float
    u0: float
    dTime: float
    Rho: float
    C: float
}

let DefaultParams = {
    A = 100.0
    B = 100.0
    D = 2.0
    u0 = 1.0
    dTime = 0.00001
    Rho = 1.0
    C = 90.0
}


let forn<'T> n (func:'T->bool) (collection:'T seq) = 
    let count = collection |> Seq.filter func |> Seq.length
    count >= n

let calculateDistances (cities:City list) = 
    let distances = Array2D.create cities.Length cities.Length 0.0
    for city in cities do
        for city1 in cities do
            let dif0 = abs(city.x-city1.x)**2.0
            let dif1 =  abs(city.y-city1.y)**2.0
            distances.[city.i,city1.i] <- sqrt (dif0 + dif1)
    let maxDistance = distances |> Seq.cast<float> |> Seq.max
    distances |> Array2D.map (fun e -> e/maxDistance)

//gets tuples of row containing element and index
let rowi row (network:float[,]) = 
    network.[row,*] |> Array.mapi (fun j e -> (e,j))

let coli col (network:float[,]) = 
    network.[*,col] |> Array.mapi (fun j e -> (e,j))

let initialize (cities:City list) parameters =
    let n = cities.Length
    let r = System.Random(System.DateTime.Now.Millisecond)
    let distances = calculateDistances cities
    let u = Array2D.init n n (fun i j -> r.NextDouble()
            //let randomU0 = float (r.Next(100) / 100)*(parameters.u0*2.0)-parameters.u0;
            //parameters.u0+randomU0
        )
    let network = Array2D.init n n (fun i j -> 0.75)
    (network,distances,u)


let singlePass (v:float[,]) (distances:float[,]) (u:float[,]) parameters = 
    let n = Array2D.length2 v
    let r = System.Random()
    
    for X in 0 .. n - 1 do
        for i in 0 .. n-1 do
            let aSum = Array.fold (fun acc (e,j) -> if i<>j then acc + e else acc) 0.0 (v |> rowi X)

            let bSum = Array.fold (fun acc (e,Y) -> if Y<>X then acc + e else acc) 0.0 (v |> rowi i)
            
            let mutable cSum = 0.0
            for x in 0 .. n-1 do
                for j in 0 .. n-1 do
                    cSum <- cSum + v.[x,j]
            cSum <- cSum - (float(n) + parameters.Rho)

            let mutable dSum =0.0
            for Y in 0 .. n-1 do
                let index1 = (n + 1+i) % n
                let index2  = (n + i-1) % n
                let dAdd = distances.[X,Y] * (v.[Y,index1] + v.[Y,index2])
                dSum <- dSum + dAdd

            //momentum of given node
            let dudt = -parameters.A*aSum - parameters.B*bSum - parameters.C*cSum - parameters.D*dSum
            u.[X,i] <- u.[X,i] + parameters.dTime*(-u.[X,i] + dudt)
            //u.[X,i] <- dudt

    for X in 0 .. n-1 do
        for i in 0 .. n-1 do
            let ui = u.[X,i]
            let divided = ui / parameters.u0
            let changeValue = tanh divided
            v.[X,i] <- 0.5 * (1.0 + changeValue)

    (*
    It seems like we don't care about energy
    let mutable EASum = 0.0
    for X in 0 .. n-1 do
        let EARowSum = v |> rowi X |> Array.fold (fun acc (e,i) -> e + acc) -1.0
        EASum <- EASum + abs EARowSum

    let mutable EBSum = 0.0
    for X in 0 .. n-1 do
        let EBColSum = v |> coli X |> Array.fold (fun acc (e,i) -> e + acc) -1.0
        EBSum <- EBSum + abs EBColSum
    EASum + EBSum
        *)
    0.0
    

//returns the path of the current solution
let currentPath network =
    let n = (network |> Array2D.length2)-1
    let path = Array.zeroCreate (n+1)
    for i in 0 .. n do
        path.[i] <- (network |> coli i) |> Array.maxBy (fun (e,index) -> e)
    path

let isFeasable path =
    let firstDuplicate = path |> Seq.ofArray |> Seq.pairwise |> Seq.tryFind (fun ((a,ai),(b,bi)) -> ai = bi)
    match firstDuplicate with
        | Some(x) -> false
        | None -> true

//calculates the distance of the current path
let calculateDistance path (distances:float[,]) =
    let mutable distance = 0
    path |> Seq.ofArray |> Seq.pairwise |> Seq.sumBy (fun ((v,i),(v1,j)) -> distances.[i,j])

let generateRandomCities n = 
    let r = System.Random()
    List.init n (fun i -> 
        { 
            i=i
            x=r.NextDouble()
            y =r.NextDouble()
        })

let initializeNetworkAndRun (parameters:HopfieldTspParams option) (n:int) =
    let cities = generateRandomCities n
    let initByDefault parameters = 
            match parameters with
                | Some(pValue) -> initialize cities pValue
                | None -> initialize cities DefaultParams
    let mutable invalidPath = false
    let mutable path = Array.init n (fun i-> (float(i),i))
    while invalidPath do
        let (network, distances,u) = initByDefault parameters
        for i in 0 .. 100 do 
            singlePass network distances u DefaultParams |> ignore
        path <- currentPath network
        invalidPath <- isFeasable path
    (cities, path)

let testParameters (pms:float[]) n = 
    
    let parameters = {
        A = DefaultParams.A
        B = DefaultParams.B
        D = pms.[0]
        u0 = DefaultParams.u0
        dTime = DefaultParams.dTime
        Rho = pms.[1]
        C = pms.[2]
    }

    let allTrialPaths = seq {
        for city in 0 .. 8 do
            let cities = generateRandomCities n
            for trials in 0 .. 20 do        
                let (network,distances,u) = initialize cities parameters
                for i in 0 .. 50 do 
                    singlePass network distances u DefaultParams |> ignore
                let path = currentPath network
                let distance = calculateDistance path distances
                let feasable = isFeasable path
                yield (path, distance, feasable)
    }
    let feasable = allTrialPaths |> Seq.filter (fun (p,dist,feasable) -> feasable = true) |> List.ofSeq
    let count = List.length feasable
    if count > 90 then
        Some(feasable |> List.averageBy (fun (p,dist,ok) ->dist))
    else
        None

let determineParameters n =
    let parametersValues = [0.1;0.01;1.0;30.0;50.0;100.0]
    let combinations = (getCombsWithRep 3 parametersValues) |> List.ofSeq
    let validParameters = combinations |> List.map (fun pms -> (pms,testParameters (Array.ofList pms) n)) |> List.filter (fun (pms,avgDist) -> avgDist.IsSome)
    validParameters

let drawTSP (cities:City list) path =
    let feasable = isFeasable path
    let cityPoints = cities |> List.map (fun c -> (c.x,c.y))
    let line = (path |> Array.map (fun (v,i) -> cities.[i])) |> Array.map (fun c -> (c.x,c.y))
    let chart = Chart.Combine [   
                    Chart.Line line
                    Chart.Point cityPoints
                ]

    let area = new ChartArea("Main")

    let control = new ChartControl(chart)
    control.Width <- 700
    control.Height  <- 500

    // Show the chart control on a top-most form
    let mainForm = new Form(Visible = true, TopMost = true, 
                            Width = 700, Height = 500)
    mainForm.Controls.Add(control)
    mainForm