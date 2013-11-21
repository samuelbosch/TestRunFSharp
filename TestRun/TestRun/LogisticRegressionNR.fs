module LogisticRegressionNR

open System
open System.IO

let makeRawDataFile numLines seed fileName =
    let ofs = new FileStream(fileName, FileMode.Create)
    let sw = new StreamWriter(ofs)

    let bValues = [| -95.0; 0.4; -0.9; 11.2 |] // hard-coded

    let rand = new Random(seed)

    for i=0 to numLines-1 do
        let age = rand.Next(35, 81); // [35,81) == [35,80]
        let sex = if (rand.NextDouble() < 0.50) then "M" else "F"
        let mutable chol = (float age) / 10.0 - 1.0

        let flip = rand.Next(2);
        if (flip = 0) then
            chol <- chol + (3.0 - 1.0) * rand.NextDouble() + 1.0
        else
            chol <- chol - (3.0 - 1.0) * rand.NextDouble() + 1.0
        if (chol > 9.9) then chol <- 9.9
        if (chol < 0.1) then chol <- 0.1

        let x0 = 1.0;
        let x1 = float age;
        let x2 = if (sex = "M") then 0.0 else 1.0
        let x3 = chol;

        let z = (bValues.[0] * x0) + (bValues.[1] * x1) + (bValues.[2] * x2) + (bValues.[3] * x3);
        let p = 1.0 / (1.0 + Math.Exp(-z));

        let die = if (p < 0.5) then 0 else 1

        sw.WriteLine(sprintf "%i %s %.2f %i" age sex chol die)
   
    sw.Close()
    ofs.Close()

let displayRawData fileName numLines =
    let ifs = new FileStream(fileName, FileMode.Open)
    let sr = new StreamReader(ifs)
    printfn "Age  Sex Chol  |  Died"
    printfn  "======================"

    seq { while true do 
            yield sr.ReadLine().Split(' ') }
    |> Seq.take numLines
    |> Seq.iter (fun tokens -> (printfn " %s   %s  %s  |  %s" tokens.[0] tokens.[1] tokens.[2] tokens.[3]))
    
    sr.Close()
    ifs.Close()
    printfn " . . ."

let countLines file =
    let ifs = new FileStream(file, FileMode.Open)
    let sr = new StreamReader(ifs)
    let line = ""
    let ct = Seq.sum (seq { while sr.ReadLine() <> null do yield 1 })
    sr.Close(); ifs.Close();
    ct

let matrixCreate rows columns = 
    Array.init rows (fun i -> Array.zeroCreate columns)
let vectorCreate rows =
    Array.zeroCreate rows

// read a file of raw age-sex-cholesterol-death (e.g., "68 M 7.56 1") and
// add an initial column of all 1.0s to correspond to the B0 constant
// the dependent Y variable (death) is loaded by a separate routine
let loadRawDataIntoDesignMatrix rawDataFile =
    let ct = countLines rawDataFile

    let result: float [][] = matrixCreate ct 4 // design col, age, sex, cholesterol

    let ifs = new FileStream(rawDataFile, FileMode.Open)
    let sr = new StreamReader(ifs)
    for row = 0 to ct-1 do 
        let line = sr.ReadLine().Trim()
        let tokens = line.Split(' ')
        let age = float tokens.[0]
        let sex = if tokens.[1] = "M" then 0.0 else 1.0
        let chol = float tokens.[2]
        result.[row].[0] <- 1.0
        result.[row].[1] <- age 
        result.[row].[2] <- sex
        result.[row].[3] <- chol
    
    sr.Close(); ifs.Close();
    result

let loadRawDataIntoYVector rawDataFile =
    let ct = countLines rawDataFile

    let result : float[] = vectorCreate ct // single column vector
    let ifs = new FileStream(rawDataFile, FileMode.Open);
    let sr = new StreamReader(ifs);
    
    for row = 0 to ct-1 do 
        let line = sr.ReadLine().Trim()
        let tokens = line.Split(' ')
        let died = if (int tokens.[3]) = 1 then 1.0 else 0.0
        result.[row] <- died 

    sr.Close(); ifs.Close();
    result

let matrixAsString (matrix:float[][]) numRows digits width =
    let acc s (row:float[]) = 
        row 
        |> Array.map (fun e -> (e.ToString("F"+(string digits)).PadLeft(width))) 
        |> fun strings -> s + (String.Join(" ",strings) + Environment.NewLine)
    
    matrix
    |> Seq.take numRows
    |> Seq.fold acc ""

let vectorAsString (vector:float[]) count digits width =
    let acc s (e:float) = 
        s + e.ToString("F"+(string digits)).PadLeft(width) + Environment.NewLine
    vector 
    |> Seq.take (min count (vector.Length))
    |> Seq.fold acc ""

let vectorDuplicate = Array.copy

let constructProbVector (xMatrix:float[][]) (bVector:float []) = 
    // p = 1 / (1 + exp(-z) where z = b0x0 + b1x1 + b2x2 + b3x3 + . . .
    // suppose X is 10 x 4 (cols are: x0 = const. 1.0, x1, x2, x3)
    // then b would be a 4 x 1 (col vecror)
    // then result of X times b is (10x4)(4x1) = (10x1) column vector

    let xRows = xMatrix.Length;
    let xCols = xMatrix.[0].Length
    let bRows = bVector.Length

    if (xCols <> bRows) then
        raise (new Exception("xMatrix and bVector are not compatible in ConstructProbVector"))

    let result = vectorCreate(xRows) // ex: if xMatrix is size 10 x 4 and bVector is 4 x 1 then prob vector is 10 x 1 (one prob for every row of xMatrix)

    let mutable z = 0.0

    for i = 0 to xRows-1 do
        z <- 0.0
        for j = 0 to xCols-1 do
            z <- z + (xMatrix.[i].[j] * bVector.[j]) // b0(1.0) + b1x1 + b2x2 + . . .
    
        let p = 1.0 / (1.0 + Math.Exp(-z))  // consider checking for huge value of Math.Exp(-z) here
        result.[i] <- p
    
    result

let meanSquaredError (pVector:float[]) (yVector:float[]) =
    // how good are the predictions? (using an already-calculated prob vector)
    // note: it is possible that a model with better (lower) MSE than a second model could give worse predictive accuracy.
    let pRows = pVector.Length
    if (pRows <> yVector.Length) then
        raise (new Exception("The prob vector and the y vector are not compatible in MeanSquaredError()"))
    if (pRows = 0) then
        0.0
    else
        let result  = Array.fold2 (fun s p y -> s + ((p - y) * (p - y))) 0.0 pVector yVector
        result / (float pRows)

let matrixInit rows cols initializer =
    Array.init rows (fun row -> 
        Array.init cols (initializer row))

let matrixTranspose (matrix:float[][]) =
    let rows = matrix.Length
    let cols = matrix.[0].Length // assume all columns have equal size
    // note the indexing swap
    matrixInit cols rows (fun j i -> matrix.[i].[j]) 

let computeXtilde (pVector:float[]) (xMatrix:float[][]) =
    // note: W[t-1] is nxn which could be huge so instead of computing b[t] = b[t-1] + inv(X'W[t-1]X)X'(y - p[t-1]) directly
    // we compute the W[t-1]X part, without the use of W.
    // Since W is derived from the prob vector and W has non-0.0 elements only on the diagonal we can avoid a ton of work
    // by using the prob vector directly and not computing W at all.
    // Some of the research papers refer to the product W[t-1]X as X~ hence the name of this method.
    // ex: if xMatrix is 10x4 then W would be 10x10 so WX would be 10x4 -- the same size as X
    let pRows = pVector.Length;
    let xRows = xMatrix.Length;
    let xCols = xMatrix.[0].Length;

    if (pRows <> xRows) then
        raise (new Exception("The pVector and xMatrix are not compatible in ComputeXtilde"))

    // we are not doing matrix multiplication. the p column vector sort of lays on top of each matrix column.
    
    let result = matrixInit pRows xCols (fun i j -> 
        pVector.[i] * (1.0 - pVector.[i]) * xMatrix.[i].[j])
    result

let matrixProduct (matrixA:float[][]) (matrixB:float[][]) =
    let aRows, aCols = matrixA.Length, matrixA.[0].Length
    let bRows, bCols = matrixB.Length, matrixB.[0].Length
    if aCols <> bRows then
        raise (new Exception("Non-conformable matrices in MatrixProduct"))
    
    let result = matrixCreate aRows bCols
    for i = 0 to aRows-1 do // each row of A
        for j = 0 to bCols-1 do // each col of B
            for k = 0 to aCols-1 do // could use k < bRows
                result.[i].[j] <- result.[i].[j] + (matrixA.[i].[k] * matrixB.[k].[j])
    result

let matrixDuplicate (matrix:float[][]) =
    Array.init matrix.Length (fun i -> Array.copy matrix.[i])

let inline swap (arr:'a []) a b = 
    let tmp = arr.[a]
    arr.[a] <- arr.[b]
    arr.[b] <- tmp

let matrixDecompose (matrix:float[][]) =
    // Doolittle's method (1.0s on L diagonal) with partial pivoting
    let rows = matrix.Length
    let cols = matrix.[0].Length; // assume all rows have the same number of columns so just use row [0].
    if (rows <> cols) then
        raise (new Exception("Attempt to MatrixDecompose a non-square mattrix"))

    let n = rows // convenience
    let result = matrixDuplicate matrix
    let perm = Array.init n id // set up row permutation result

    let rec loop tog j = // toggle tracks number of row swaps. used by MatrixDeterminant
        if j >= n-1 then // each column
            Some(result), perm , tog
        else
            let mutable max, pRow = Math.Abs(result.[j].[j]), j // find largest value in row
        
            for i = j + 1 to n-1 do
                let aij = Math.Abs(result.[i].[j])
                if aij > max then
                    max <- aij 
                    pRow <- i

            let tog = if pRow <> j then // if largest value not on pivot, swap rows
                        swap result pRow j
                        swap perm pRow j // and swap perm info
                        -tog // adjust the row-swap toggle
                      else tog
        
            let ajj = result.[j].[j]
            if (abs ajj < 0.00000001) then // if diagonal after swap is zero . . .
                None, perm, tog 
            else
                for i = j + 1 to n - 1 do
                    let aij = result.[i].[j] / ajj
                    result.[i].[j] <- aij
                    for k = j + 1 to n - 1 do
                        result.[i].[k] <- result.[i].[k] - (aij * result.[j].[k])    
                loop tog (j+1)
    loop 1 0

let helperSolve (luMatrix:float[][]) (b:float[]) =
    // solve Ax = b if you already have luMatrix from A and b has been permuted
    let n = luMatrix.Length

    // 1. make a copy of the permuted b vector
    let x = Array.copy b

    // 2. solve Ly = b using forward substitution
    for i in 1 .. n-1 do
        let mutable sum = x.[i]
        for j in 0 .. i-1 do
             sum <- sum - (luMatrix.[i].[j] * x.[j])
        x.[i] <- sum

    // 3. solve Ux = y using backward substitution
    x.[n-1] <- x.[n-1] / luMatrix.[n - 1].[n - 1]
    for i in n - 2 .. -1 .. 0 do 
        let mutable sum = x.[i]

        for j in i + 1 .. n - 1 do
            sum <- sum - (luMatrix.[i].[j] * x.[j])
        
        x.[i] <- sum / luMatrix.[i].[i]
    x

let matrixInverse (matrix:float[][]) =
    let n = matrix.Length
    let lum, (perm:int []), (toggle:int) = matrixDecompose matrix
    match lum with 
    | None -> None
    | Some(lum) ->
        let result = matrixDuplicate matrix
        let b = Array.zeroCreate n
        for i in 0 .. n - 1 do
            for j in 0 .. n - 1 do
                b.[j] <- if i = perm.[j] then 1.0 else 0.0
            let x = helperSolve lum b
            for j in 0 .. n - 1 do
                result.[j].[i] <- x.[j]
        Some(result)

let vectorOp op (vectorA:float[]) (vectorB:float[]) =
    let aRows, bRows = vectorA.Length, vectorB.Length
    if aRows <> bRows then
        raise (new Exception("Non-conformable vectors in VectorSubtraction"))
    Array.map2 op vectorA vectorB

let vectorSubstraction =  vectorOp (-)
let vectorAddition =  vectorOp (+)

let matrixVectorProduct (matrix:float[][]) (vector:float[]) =
    let mRows, mCols = matrix.Length, matrix.[0].Length
    let vRows = vector.Length
    if mCols <> vRows then
        raise (new Exception("Non-conformable matrix and vector in MatrixVectorProduct"))
    
    matrix |> Array.map
        (Array.mapi (fun j e -> e * vector.[j]) >> Array.sum)

let constructNewBetaVector (oldBetaVector:float[]) (xMatrix:float[][]) (yVector:float[]) (oldProbVector:float[]) =
    // this is the heart of the Newton-Raphson technique
    // b[t] = b[t-1] + inv(X'W[t-1]X)X'(y - p[t-1])
    //
    // b[t] is the new (time t) b column vector
    // b[t-1] is the old (time t-1) vector
    // X' is the transpose of the X matrix of x data (1.0, age, sex, chol)
    // W[t-1] is the old weight matrix
    // y is the column vector of binary dependent variable data
    // p[t-1] is the old column probability vector (computed as 1.0 / (1.0 + exp(-z) where z = b0x0 + b1x1 + . . .)

    // note: W[t-1] is nxn which could be huge so instead of computing b[t] = b[t-1] + inv(X'W[t-1]X)X'(y - p[t-1])
    // compute b[t] = b[t-1] + inv(X'X~)X'(y - p[t-1]) where X~ is W[t-1]X computed directly
    // the idea is that the vast majority of W[t-1] cells are 0.0 and so can be ignored

    let Xt = matrixTranspose xMatrix                 // X'
    let A = computeXtilde oldProbVector xMatrix      // WX
    let B = matrixProduct Xt A                       // X'WX

    match matrixInverse B with // inv(X'WX)
    | None -> None             // computing the inverse can blow up easily
    | Some(C) ->
        let D = matrixProduct C Xt                        // inv(X'WX)X'
        let YP = vectorSubstraction yVector oldProbVector  // y-p
        let E = matrixVectorProduct D YP                  // inv(X'WX)X'(y-p)
        let result = vectorAddition oldBetaVector E       // b + inv(X'WX)X'(y-p)
        Some(result)

let noChange (oldBvector:float[]) newBvector epsilon =
    // true if all new b values have changed by amount smaller than epsilon
    oldBvector |> Array.forall2 (fun a b -> (Math.Abs(a-b)) < epsilon) <| newBvector

let outOfControl (oldBvector:float[]) newBvector jumpFactor =
    // true if any new b is jumpFactor times greater than old b
    (Array.forall ((<>) 0.0) oldBvector)
    &&
    oldBvector |> Array.exists2 (fun a b -> ((Math.Abs(a - b) / Math.Abs(a)) > jumpFactor)) <| newBvector

let computeBestBeta (xMatrix:float [][]) (yVector:float[]) maxIterations epsilon jumpFactor =
    // Use the Newton-Raphson technique to estimate logistic regression beta parameters
    // xMatrix is a design matrix of predictor variables where the first column is augmented with all 1.0 to represent dummy x values for the b0 constant
    // yVector is a column vector of binary (0.0 or 1.0) dependent variables
    // maxIterations is the maximum number of times to iterate in the algorithm. A value of 1000 is reasonable.
    // epsilon is a closeness parameter: if all new b[i] values after an iteration are within epsilon of
    // the old b[i] values, we assume the algorithm has converged and we return. A value like 0.001 is often reasonable.
    // jumpFactor stops the algorithm if any new beta value is jumpFactor times greater than the old value. A value of 1000.0 seems reasonable.
    // The return is a column vector of the beta estimates: b[0] is the constant, b[1] for x1, etc.
    // There is a lot that can go wrong here. The algorithm involves finding a matrx inverse (see MatrixInverse) which will throw
    // if the inverse cannot be computed. The Newton-Raphson algorithm can generate beta values that tend towards infinity. 
    // If anything bad happens the return is the best beta values known at the time (which could be all 0.0 values but not null).
    
    let xRows = xMatrix.Length
    let xCols = xMatrix.[0].Length

    if (xRows <> yVector.Length) then
        raise (new Exception("The xMatrix and yVector are not compatible in LogisticRegressionNewtonParameters()"))

    // initial beta values
    let bVector:float[] = Array.zeroCreate xCols
    
    // best beta values found so far
    let bestBvector = vectorDuplicate bVector

    let pVector = constructProbVector xMatrix bVector // a column vector of the probabilities of each row using the b[i] values and the x[i] values.
    
    let mse = meanSquaredError pVector yVector
    let timesWorse = 0 // how many times are the new betas worse (i.e., give worse MSE) than the current betas

    let rec loop (bVector:float[]) (bestBvector:float[]) (pVector:float[]) (mse:float) timesWorse i = 
        if i < maxIterations then
            let newBvector = constructNewBetaVector bVector xMatrix yVector pVector // generate new beta values using Newton-Raphson. could return null.
            match newBvector with
            | None -> bestBvector
            | Some(newBvector) ->
                // no significant change?
                if noChange bVector newBvector epsilon then
                    bestBvector
                // spinning out of control?
                else if outOfControl bVector newBvector jumpFactor then
                    bestBvector
                else
                    let pVector = constructProbVector xMatrix newBvector
                    
                    // are we getting worse or better?
                    let newMSE = meanSquaredError pVector yVector // smaller is better
                    if newMSE > mse then // new MSE is worse than current SSD
                        let timesWorse = timesWorse + 1 // update counter
                        if timesWorse >= 4 then
                            bestBvector
                        else
                            let bVector = 
                                newBvector // update current b: old b becomes not the new b but halfway between new and old
                                |> Array.mapi (fun k newB -> (bVector.[k] + newB ) / 2.0)
                            let mse = newMSE
                            loop bVector bestBvector pVector mse timesWorse (i+1)
                    else // new SSD is be better than old
                        let bVector = vectorDuplicate newBvector
                        let bestBvector = vectorDuplicate bVector
                        let mse = newMSE
                        let timesWorse = 0
                        loop bVector bestBvector pVector mse timesWorse (i+1)
        else
            bestBvector

    loop bVector bestBvector pVector mse timesWorse 0      

let predictiveAccuracy (xMatrix:float[][]) (yVector:float[]) (bVector:float[]) =
    // returns the percent (as 0.00 to 100.00) accuracy of the bVector measured by how many lines of data are correctly predicted.
    // note: this is not the same as accuracy as measured by sum of squared deviations between 
    // the probabilities produceed by bVector and 0.0 and 1.0 data in yVector
    // For predictions we simply see if the p produced by b are >= 0.50 or not. 
    let xRows, xCols = xMatrix.Length, xMatrix.[0].Length
    let yRows, bRows = yVector.Length,bVector.Length
    if (xCols <> bRows || xRows <> yRows) then
        raise (new Exception("Bad dimensions for xMatrix or yVector or bVector in PredictiveAccuracy()"))
    let pVector = constructProbVector xMatrix bVector
    let pRows = pVector.Length;
    if pRows <> xRows then
        raise (new Exception("Unequal rows in prob vector and design matrix in PredictiveAccuracy()"))

    
    let numberCasesCorrect = 
        Array.map2 (fun y p -> if (y=1.0)= (p>=0.5) then 1. else 0.) yVector pVector
        |> Array.sum
    let numberCasesWrong = (float yVector.Length) - numberCasesCorrect
    let total = numberCasesCorrect + numberCasesWrong
    if total = 0. then
        0.
    else
        (100. * numberCasesCorrect) / total

let run() =
    printfn "\nBegin Logistic Regression with Newton-Raphson demo"

    let trainFile = "trainFile.txt"  // will be placed in same directory as executable
    let testFile = "testFile.txt"

    printfn "\nCreating 80 lines of synthetic training data and 20 lines of test data"
    makeRawDataFile 80 3 trainFile
    makeRawDataFile 20 4 testFile

    printfn "\nFirst 5 lines of training data file are: \n"
    displayRawData trainFile 5

    printfn "\nLoading train and test data from files into memory"

    let xTrainMatrix = loadRawDataIntoDesignMatrix trainFile
    printfn "\nFirst five rows of x training matrix:"
    printfn "%s" (matrixAsString xTrainMatrix 5 1 5)

    let yTrainVector = loadRawDataIntoYVector(trainFile)
    Console.WriteLine("\nFirst five rows of y training vector:")
    Console.WriteLine(vectorAsString yTrainVector 5 1 3)

    let xTestMatrix = loadRawDataIntoDesignMatrix testFile
    let yTestVector = loadRawDataIntoYVector testFile

    Console.WriteLine("Setting Newton-Raphson algorithm stop conditions")
    let maxIterations = 25
    let epsilon = 0.01 // stop if all new beta values change less than epsilon (algorithm has converged?)
    let jumpFactor = 1000.0 // stop if any new beta jumps too much (algorithm spinning out of control?)
    printfn "\nmaxIterations = %i epsilon = %g jumpFactor = %g" maxIterations epsilon jumpFactor

    Console.WriteLine("\nUsing Newton-Raphson to find beta parameters that best fit the training data")
    let beta = computeBestBeta xTrainMatrix yTrainVector maxIterations epsilon jumpFactor // computing the beta parameters is synonymous with 'training'
    Console.WriteLine("\nNewton-Raphson complete");
    Console.WriteLine("\nThe beta vector is: ");
    Console.WriteLine(vectorAsString beta (Int32.MaxValue) 4 10)

    Console.WriteLine("Computing accuracy on test data using the beta values")
    let acc = predictiveAccuracy xTestMatrix yTestVector beta // percent of data cases correctly predicted in the test data set.
    printfn "\nThe predictive accuracy of the model on the test data is %2f%%\n" acc

    Console.WriteLine("\nEnd demo");
