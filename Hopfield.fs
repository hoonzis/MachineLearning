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
    u

let sumAllBut (i:int) (values:(float*int)[]) = 
    Array.fold (fun acc (e,j) -> if i<>j then acc + e else acc) 0.0 values

//calculates the value of node from input potential
let vValue (ui:float) (parameters:HopfieldTspParams) = (1.0 + tanh(ui*parameters.alfa))/2.0

let dSumCalc distances city position (v:float[,]) = 
    let n = v |> Array2D.length1
    (distances |> rowi city) |> Array.sumBy (fun (e,i) -> 
        let index1 = (n+position+1) % n
        let index2  = (n+position-1) % n
        e*(v.[i,index1] + v.[i,index2])
    )

let toValues (pms:HopfieldTspParams) (u:(float*int)[]) = 
    u|> Array.map (fun (ui,i) -> vValue ui pms,i)

let singlePass (distances:float[,]) (u:float[,]) pms city position = 
    let n = Array2D.length2 u
    
    let aSum = sumAllBut position (u |> rowi city |> toValues pms)

    let bSum = sumAllBut city (u |> coli position |> toValues pms)
            
    let cSum = (u |> Seq.cast<float> |> Seq.map (fun ui -> vValue ui pms) |> Seq.sum) - float(n+1)

    let dSum = dSumCalc distances city position (u|> Array2D.map (fun i-> vValue i pms))

    //momentum of given node
    let dudt = -pms.A*aSum - pms.B*bSum - pms.C*cSum - pms.D*dSum
    //u.[city,position] <- u.[city,position] + parameters.dTime*(-u.[city,position] + dudt)
    u.[city,position] <- dudt

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
    path |> Seq.ofArray |> Seq.pairwise |> Seq.sumBy (fun ((v,i),(v1,j)) -> distances.[i,j])

let generateRandomCities n = 
    let r = System.Random()
    List.init n (fun i -> 
        { 
            i=i
            x=r.NextDouble()
            y =r.NextDouble()
        })

let initAndRunUntilStable cities pms distances = 
    let r = new Random()
    let u = initialize cities pms
    let mutable continueLooping = true
    let mutable iterCount = 0
    while continueLooping do
        let city = r.Next()
        let position = r.Next()
        singlePass distances u pms city position
        iterCount <- iterCount + 1
        continueLooping <- iterCount < 100000
    let network = u |> Array2D.map (fun e -> vValue e pms)
    network

let initializeNetworkAndRun (pms:HopfieldTspParams ) (n:int) =
    let cities = generateRandomCities n
    let distances = calculateDistances cities
    let networks = Seq.initInfinite (fun i -> initAndRunUntilStable cities pms distances)
    let paths = networks |> Seq.map (fun v-> currentPath v)
    let validPath = paths |> Seq.find (fun path -> isFeasable path)
    cities, validPath

let paramsFromArray (pms:float[]) = 
    let parameters = {
        A = pms.[0]
        B = pms.[1]
        D = pms.[2]
        alfa = 50.0
        dTime = 0.00001
        Rho = 1.0
        C = pms.[3]
    }
    parameters

let testParameters (pms:float[]) n = 
    let parameters = paramsFromArray pms

    let allTrialPaths = seq {
        let trialsCount = ref 0
        let feasableCount = ref 0
        for city in 0 .. 5 do
            let cities = generateRandomCities n
            let distances = calculateDistances cities
            for trials in 1 .. 5 do
                let v = initAndRunUntilStable cities parameters distances
                let path = currentPath v
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
    float(count)/110.0, avgRouteSize
    
let determineParameters n pmsRange =
    let combinations = (getCombsWithRep 4 pmsRange) |> List.ofSeq
    let validParameters = combinations |> List.map (fun pms -> (pms,testParameters (Array.ofList pms) n)) |> List.map (fun (pms,distAndRate) -> pms,fst(distAndRate),snd(distAndRate))
    validParameters |> List.sortBy (fun (pms,convRate, avgRoute) -> convRate) |> List.rev |> Seq.toList

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