module LogisticRegressionNR

open System
open System.IO

let makeRawDataFile numLines seed fileName =
    let ofs = new FileStream(fileName, FileMode.Create)
    let sw = new StreamWriter(ofs)

    let bValues = [| -95.0; 0.4; -0.9; 11.2 |] // hard-coded

    let rand = new Random(seed)

    for i=0 to numLines do
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

        sw.WriteLine(sprintf "%i %s %s %i" age sex (chol.ToString("F2")) die)
   
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
    for row = 0 to ct do 
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
    
    for row = 0 to ct do 
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
    Array.fold acc "" vector

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

    for i = 0 to xRows do
        z <- 0.0
        for j = 0 to xCols do
            z <- z + xMatrix.[i].[j] * bVector.[j] // b0(1.0) + b1x1 + b2x2 + . . .
    
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

    //CONTINUE HERE!
//    let iteration = 
//        newBVector = constructNewBetaVector

//    for (int i = 0; i < maxIterations; ++i)
//    {
//        //Console.WriteLine("=================================");
//        //Console.WriteLine(i);
//
//        double[] newBvector = ConstructNewBetaVector(bVector, xMatrix, yVector, pVector); // generate new beta values using Newton-Raphson. could return null.
//        if (newBvector == null)
//        {
//            //Console.WriteLine("The ConstructNewBetaVector() helper method in LogisticRegressionNewtonParameters() returned null");
//            //Console.WriteLine("because the MatrixInverse() helper method in ConstructNewBetaVector returned null");
//            //Console.WriteLine("because the current (X'X~) product could not be inverted");
//            //Console.WriteLine("Returning best beta vector found");
//            //Console.ReadLine();
//            return bestBvector;
//        }
//
//        //Console.WriteLine("New b vector is ");
//        //Console.WriteLine(VectorAsString(newBvector)); Console.WriteLine("\n");
//
//        // no significant change?
//        if (NoChange(bVector, newBvector, epsilon) == true) // we are done because of no significant change in beta[]
//        {
//            //Console.WriteLine("No significant change between old beta values and new beta values -- stopping");
//            //Console.ReadLine();
//            return bestBvector;
//        }
//        // spinning out of control?
//        if (OutOfControl(bVector, newBvector, jumpFactor) == true) // any new beta more than jumpFactor times greater than old?
//        {
//            //Console.WriteLine("The new beta vector has at least one value which changed by a factor of " + jumpFactor + " -- stopping");
//            //Console.ReadLine();
//            return bestBvector;
//        }
//
//        pVector = ConstructProbVector(xMatrix, newBvector);
//
//        // are we getting worse or better?
//        double newMSE = MeanSquaredError(pVector, yVector); // smaller is better
//        if (newMSE > mse) // new MSE is worse than current SSD
//        {
//            ++timesWorse;           // update counter
//            if (timesWorse >= 4)
//            {
//            //Console.WriteLine("The new beta vector produced worse predictions even after modification four times in a row -- stopping");
//            return bestBvector;
//            }
//            //Console.WriteLine("The new beta vector has produced probabilities which give worse predictions -- modifying new betas to halfway between old and new");
//            //Console.WriteLine("Times worse = " + timesWorse);
//
//            bVector = VectorDuplicate(newBvector);   // update current b: old b becomes not the new b but halfway between new and old
//            for (int k = 0; k < bVector.Length; ++k) { bVector[k] = (bVector[k] + newBvector[k]) / 2.0; }
//            mse = newMSE;                            // update current SSD (do not update best b because we don't have a new best b)
//            //Console.ReadLine();
//        }
//        else // new SSD is be better than old
//        {
//            bVector = VectorDuplicate(newBvector);  // update current b: old b becomes new b
//            bestBvector = VectorDuplicate(bVector); // update best b
//            mse = newMSE;                           // update current MSE
//            timesWorse = 0;                         // reset counter
//        }
//
//    } // end main iteration loop

    bestBvector


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
    Console.WriteLine(vectorAsString beta (int.MaxValue) 4 10)

    Console.WriteLine("Computing accuracy on test data using the beta values")
    let acc = predictiveAccuracy xTestMatrix yTestVector beta // percent of data cases correctly predicted in the test data set.
    printfn "\nThe predictive accuracy of the model on the test data is %s%\n" (acc.ToString("F2"))

    Console.WriteLine("\nEnd demo");
