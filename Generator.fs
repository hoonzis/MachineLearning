module MachineLearning.Generator

open System
open MathNet.Numerics.Random
open MathNet.Numerics.Distributions


let dist1 = Normal(0.0, 1.0, RandomSource = Random(100))
let dist2 = Normal(0.0, 2.0, RandomSource = Random(100))

let rec randomWalk value (dist:IContinuousDistribution) = seq {
    yield value
    yield! randomWalk (value + dist.Sample()) dist
}

let randomPrice drift volatility dt initial (dist:Normal) = 
    // Calculate parameters of the exponential
    let driftExp = (drift - 0.5 * pown volatility 2) * dt
    let randExp = volatility * (sqrt dt)

    // Recursive loop that actually generates the price
    let rec loop price = seq {
        yield price
        let price = price * exp (driftExp + randExp * dist.Sample()) 
        yield! loop price }

    // Return path starting at 'initial'
    loop initial