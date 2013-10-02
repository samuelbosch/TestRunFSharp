module RadialNetIO

open System

// based on http://msdn.microsoft.com/en-us/magazine/dn451445.aspx (October 2013 Issue)

let centroids = [|
      [| -3.0; -3.5; -3.8 |] ;
      [| -1.0; -1.5; -1.8 |];
      [| 2.0; 2.5; 2.8 |];
      [| 4.0; 4.5; 4.8 |];
    |]
let stdevs = [| 2.22; 3.33; 4.44; 5.55 |]

let hoWeights = [|
      [| 5.0; -5.1 |];
      [| -5.2; 5.3 |];
      [| -5.4; 5.5 |];
      [| 5.6; -5.7 |];
    |]

let biases = [|7.0; 7.1 |]

let distanceSquared x c =
    Array.fold2 (fun sum x c -> (sum+((x-c)*(x-c)))) 0.0  x c
    //slower alternative but slightly more readable
    //Array.map2 (fun x c -> ((x-c)*(x-c))) x c
    //|> Array.sum 

let kOutput hOutputs hoWeightsColumn = 
    Array.fold2 (fun sum hOutput hoWeight -> (sum + (hOutput * hoWeight))) 0.0 hOutputs hoWeightsColumn

let print msg arr =
    printfn msg arr
    arr

let computeOutputs numHidden numOutput inputs = 
    let hOutputs = 
        Array.map (fun c -> distanceSquared inputs c) centroids 
        |> Array.map2 (fun stdev d -> (-1.0 * d / (2.0 * stdev * stdev))) stdevs 
        |> Array.map Math.Exp

    let hoWeightsTransposed = Array.init numOutput (fun x  -> (Array.init numHidden (fun y ->  hoWeights.[y].[x])))
    let result = 
        Array.map (kOutput hOutputs) hoWeightsTransposed
        |> Array.map2 (+) biases
    result

let run() = 
    printfn "Begin Radial Basis Function (RBF) network demo"
    let numInput = 3
    let numHidden = 4
    let numOutput = 2
    printfn "Creating %i-%i-%i radial net" numInput numHidden numOutput
    let xValues = [| 1.0; -2.0; 3.0 |]
    printfn "x-input %A" xValues
    let yValues = computeOutputs numHidden numOutput xValues
    printfn "Output %A" yValues

    RadialNetworksInputOutput.RadialNetIOProgram.Main(null)


