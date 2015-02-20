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
    alfa: float
    dTime: float
    Rho: float
    C: float
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
    let u = Array2D.init n n (fun i j -> r.NextDouble())
    let network = Array2D.init n n (fun i j -> 0.75)
    (network,u)

let sumAllBut (i:int) (values:(float*int)[]) = 
    Array.fold (fun acc (e,j) -> if i<>j then acc + e else acc) 0.0 values

let dSumCalc distances (v:float[,]) city position n = 
    (distances |> rowi city) |> Array.sumBy (fun (e,i) -> 
        let index1 = (n+position+1) % n
        let index2  = (n+position-1) % n
        e*(v.[i,index1] + v.[i,index2])
    )
    
let singleRandomPass (v:float[,]) (distances:float[,]) (u:float[,]) parameters (r:System.Random) netWorkValue = 
    let n = Array2D.length2 v
    let city = r.Next(n)
    let position = r.Next(n)
    
    let aSum = sumAllBut position (v |> rowi city)
    let bSum = sumAllBut city (v |> coli position)
            
    let cSum = (v |> Seq.cast<float> |> Seq.sum) - float(n)

    let dSum = dSumCalc distances v city position n

    //momentum of given node
    let dudt = -parameters.A*aSum - parameters.B*bSum - parameters.C*cSum - parameters.D*dSum
    u.[city,position] <- u.[city,position] + parameters.dTime*(-u.[city,position] + dudt)
    //u.[X,i] <- dudt
    let ui = u.[city,position]
    let changeValue = tanh(ui*parameters.alfa)
    let oldVXi = v.[city,position]
    v.[city,position] <- 0.5 * (1.0 + changeValue)
    netWorkValue + v.[city,position] - oldVXi

//returns the path of the current solution
let currentPath network =
    let n = (network |> Array2D.length2)-1
    let path = Array.zeroCreate (n+1)
    for i in 0 .. n do
        path.[i] <- (network |> coli i) |> Array.maxBy (fun (e,index) -> e)
    path

let isFeasable path =
    let counts = path |> Seq.countBy (fun (c,e)->e) |> List.ofSeq
    not (counts |> Seq.exists (fun (e,i) -> i>1))

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

let notStable (n:int) nValue = if nValue < 0.85*float(n*n) && nValue >= 1.05*float(n) then true else false

let initAndRunUntilStable cities pms distances n = 
    let r = new Random()
    let (network,u) = initialize cities pms
    let mutable nValue = network |> Seq.cast<float> |> Seq.sum
    let mutable continueLooping = true
    let mutable iterCount = 0
    while continueLooping do
        nValue <- singleRandomPass network distances u pms r nValue
        iterCount <- iterCount + 1
        continueLooping <- (notStable n nValue) && iterCount < 5000
    network

let initializeNetworkAndRun (pms:HopfieldTspParams ) (n:int) =
    let cities = generateRandomCities n
    let distances = calculateDistances cities
    let maxNaValue = float(n*n)
    let paths = seq {
        for i in 0..100 do
            let network = initAndRunUntilStable cities pms distances n
            let path = currentPath network
            yield path
    }
    
    
    let minPath = paths |> Seq.filter (fun p-> isFeasable p) |> Seq.minBy (fun p -> calculateDistance p distances)
    cities,minPath

let paramsFromArray (pms:float[]) = 
    let parameters = {
        A = pms.[0]
        B = pms.[1]
        D = pms.[2]
        alfa = pms.[3]
        dTime = pms.[4]
        Rho = 1.0
        C = pms.[5]
    }
    parameters

let testParameters (pms:float[]) n = 
    let parameters = paramsFromArray pms

    let allTrialPaths = seq {
        let trialsCount = ref 0
        let feasableCount = ref 0
        for city in 0 .. 20 do
            let cities = generateRandomCities n
            let distances = calculateDistances cities
            for trials in 1 .. 10 do
                let network = initAndRunUntilStable cities parameters distances n
                let path = currentPath network
                let distance = calculateDistance path distances
                let feasable = isFeasable path
                if feasable then incr feasableCount
                incr trialsCount
                let currentRate = float(!feasableCount)/float(!trialsCount)
                yield (path, distance, feasable, currentRate, !trialsCount)
    }
      
    let feasable = allTrialPaths 
                    |> Seq.takeWhile (fun (p,dist,feasable, rate, trials) -> rate > 0.45 || trials<30) 
                    |> Seq.map (fun (p,dist,feasable, rate, trials) -> (p,dist,feasable)) 
                    |> Seq.filter (fun (p,dist,feasable) -> feasable = true) 
                    |> List.ofSeq

    let count = List.length feasable
    //return a tuple of conversionRate and avgRouteSize
    let avgRouteSize = if count > 0 then feasable |> List.averageBy (fun (p,dist,ok) ->dist) else 0.0
    float(count)/220.0, avgRouteSize
    
let determineParameters n pmsRange =
    let combinations = (getCombsWithRep 6 pmsRange) |> List.ofSeq
    let validParameters = combinations |> List.map (fun pms -> (pms,testParameters (Array.ofList pms) n)) |> List.map (fun (pms,distAndRate) -> pms,fst(distAndRate),snd(distAndRate))
    validParameters |> List.sortBy (fun (pms,convRate, avgRoute) -> convRate) |> List.rev |> Seq.take 20 |> Seq.toList

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