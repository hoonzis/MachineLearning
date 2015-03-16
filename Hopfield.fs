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

type IterationType =
    | Serial
    | Random

type HopfieldTspParams = {
    A: float
    B: float
    D: float
    alfa: float
    dTime: float
    Rho: float
    C: float
    Update:IterationType
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
let v (ui:float) (parameters:HopfieldTspParams) = (1.0 + tanh(ui*parameters.alfa))/2.0

let dSumCalc distances city position (v:float[,]) = 
    let n = v |> Array2D.length1
    (distances |> rowi city) |> Array.sumBy (fun (e,i) -> 
        let index1 = (n+position+1) % n
        let index2  = (n+position-1) % n
        e*(v.[i,index1] + v.[i,index2])
    )

let toValues (pms:HopfieldTspParams) u = 
    u|> Array2D.map (fun (ui) -> v ui pms)

//updates a single node in input potential matrix
let singlePass (distances:float[,]) (u:float[,]) pms city position = 
    let n = Array2D.length2 u
    
    let values = u |> toValues pms

    let aSum = sumAllBut position (values |> rowi city)

    let bSum = sumAllBut city (values |> coli position)
            
    let cSum = (values |> Seq.cast<float> |> Seq.sum) - float(n+1)

    let dSum = dSumCalc distances city position values

    //momentum of given node
    let dudt = -pms.A*aSum - pms.B*bSum - pms.C*cSum - pms.D*dSum
    let r = u.[city,position] + pms.dTime*(-u.[city,position] + dudt)
    //let r = dudt
    r

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



//go over all nodes in the input potential matrix and update the value
let serialIteration u pms distances = 
    u |> Array2D.mapi (fun i j x -> singlePass distances u pms i j)

//randomly pick nodes and update input potential matrix and update the value
let randomIteration u pms distances = 
    let r = new Random(DateTime.Now.Millisecond)
    let n = Array2D.length1 u
    for i in 0 .. 1000*n do
        let city = r.Next(n)
        let position = r.Next(n)
        u.[city, position] <- singlePass distances u pms city position
    u

let initAndRunUntilStable cities pms distances = 
    let u = initialize cities pms
    {1 .. 20} |> Seq.fold (fun uNext i -> 
            match pms.Update with
                | Random -> randomIteration uNext pms distances
                | Serial -> serialIteration uNext pms distances
    ) u
    
let sampleRun (pms:HopfieldTspParams ) (n:int) =
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
        alfa = 500.0
        dTime = 0.00001
        Rho = 1.0
        C = pms.[3]
        Update = Random
    }
    parameters

let third (_, _,c,_) = c;

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
                let feasable = isFeasable path
                incr trialsCount
                if feasable then
                    incr feasableCount
                    let distance = calculateDistance path distances
                    let currentRate = float(!feasableCount)/float(!trialsCount)
                    yield (path, distance, currentRate, !trialsCount)
    }
    
    //this is like prunning the results. if after 15 trials the succ. rate is still lower than 0.25, probably the config is not worth other exploration
    let feasable = allTrialPaths 
                    |> Seq.takeWhile (fun (p,dist,rate, trials) -> rate > 0.25 || trials<15) 
                    |> List.ofSeq
                    |> List.rev

    let feasableCount = List.length feasable
    let rate =  if feasableCount > 0 then third(List.head feasable) else 0.0
    let avgRouteSize = if feasableCount > 0 then feasable |> List.averageBy (fun (p,dist,rate,trials) -> dist) else 0.0
    rate, avgRouteSize
    
let determineParameters n pmsRange =
    let combinations = (getCombsWithRep 4 pmsRange) |> List.ofSeq
    let validParameters = combinations |> List.map (fun pms -> (pms,testParameters (Array.ofList pms) n)) |> List.map (fun (pms,distAndRate) -> pms,fst(distAndRate),snd(distAndRate))
    validParameters |> List.sortBy (fun (pms,convRate, avgRoute) -> convRate) |> List.rev |> Seq.toList

let drawTSP (cities:City list) path =
    let feasable = isFeasable path
    let cityPoints = cities |> List.map (fun c -> (c.x,c.y))
    let line = (path |> Array.map (fun (v,i) -> cities.[i])) |> Array.map (fun c -> (c.x,c.y))
    //just add the first element to the line again to make circle
    let circle = Array.append line [|line.[0]|]

    let chart = Chart.Combine [   
                    Chart.Line circle
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