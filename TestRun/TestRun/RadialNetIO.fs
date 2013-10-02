module RadialNetIO

open System

let distance (x:float []) (c:float []) =
    Array.fold2 (fun sum x c -> (sum+((x-c)*(x-c)))) 0.0  x c
    |> Math.Sqrt
    //slower alternative but slightly more readable
    //Array.map2 (fun x c -> ((x-c)*(x-c))) x c
    //|> Array.sum 
    //|> Math.Sqrt

let run() = 
    let r = new Random();
    let a = Array.init 10000000 (fun i -> r.NextDouble())
    let b = Array.init 10000000 (fun i -> r.NextDouble())
    printfn "start"
    let sw = System.Diagnostics.Stopwatch.StartNew()
    printfn "%f %i" (RadialNetworksInputOutput.RadialNet.Distance(a,b)) sw.ElapsedMilliseconds
    sw.Restart()
    printfn "%f %i" (distance a b) sw.ElapsedMilliseconds

    printfn "Begin Radial Basis Function (RBF) network demo"
    let numInput = 3
    let numHidden = 4
    let numOutput = 2
    printfn "Creating %i-%i-%i radial net" numInput numHidden numOutput


