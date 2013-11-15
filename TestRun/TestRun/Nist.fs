// based on http://msdn.microsoft.com/en-us/magazine/dn520240.aspx (November 2013)

module Nist

open System
open System.Collections

module Seq =
    open System.Collections
    let ofBitArray (bitArray:BitArray) = seq { 
        for i=0 to bitArray.Length-1 do yield bitArray.Get(i) 
    }

module GammaFunctions =
    let LogGamma x =
      // Log of Gamma from Lanczos with g=5, n=6/7
      // not in A & S 
      let coef = [| 76.18009172947146; -86.50532032941677;
                    24.01409824083091; -1.231739572450155;
                    0.1208650973866179E-2; -0.5395239384953E-5 |] 
      let logSqrtTwoPi = 0.91893853320467274178
      let denom = x + 1.0
      let y = x + 5.5
      let series = 1.000000000190015;

      let calc (series, denom) coef = (series + (coef/denom)), denom+1.0
      let series, denom = Array.fold calc (series, denom) coef

      (logSqrtTwoPi + (x + 0.5) * Math.Log(y) - y + Math.Log(series / x))

    let gammaLowerSer a x =
        // incomplete gamma lower (computed by series expansion)
        if ( x < 0.0) then
            raise (new ArgumentException("x param less than 0.0 in GammaLowerSer"))

        let gln = LogGamma(a)
        let mutable n = 1
        let mutable ap = a
        let mutable del = 1.0 / a
        let mutable sum = del
        let mutable result = 0.0
        while n <= 100 && result = 0.0 do
            ap <- ap+1.0
            del <- del * (x/ap)
            sum <- sum + del
            if (Math.Abs(del) < Math.Abs(sum) * 3.0E-7) then // close enough?
                result <- sum * Math.Exp(-x + a * Math.Log(x) - gln)
            else 
                n <- n + 1
        if n > 100 then
            raise (new Exception("Unable to compute GammaLowerSer to desired accuracy"))
        else
            result
        
    let gammaUpperCont a x =
        // incomplete gamma upper computed by continuing fraction
        if (x < 0.0) then
            raise (new ArgumentException("x param less than 0.0 in GammaUpperCont"))

        let mutable i = 1.0
        let gln = LogGamma(a)
        let mutable b = x + 1.0 - a
        let mutable c = 1.0 / 1.0E-30 // div by close to double.MinValue
        let mutable d = 1.0 / b
        let mutable h = d
        let mutable result = 0.0
        while i <= 100.0 && result = 0.0 do
            let an = -i * (i - a)
            b <- b + 2.0
            d <- an * d + b
            if (Math.Abs(d) < 1.0E-30) then d <- 1.0E-30 // got too small?
            c <- b + an / c
            if (Math.Abs(c) < 1.0E-30) then c <- 1.0E-30
            d <- 1.0 / d
            let del = d*c
            h <- h * del
            if (Math.Abs(del - 1.0) < 3.0E-7) then
                result <- Math.Exp(-x + a * Math.Log(x) - gln) * h
            else
                i <- i+1.0
        if i > 100.0 then
            raise (new Exception("Unable to compute GammaUpperCont to desired accuracy"))
        else
            result

    let gammaLower a x = 
        // incomplete Gamma 'P' (lower) aka 'igam'
        if (x < 0.0 || a <= 0.0) then 
            raise (new ArgumentException("Bad args in GammaLower"))
        if (x < a + 1.0) then
            gammaLowerSer a x // no surprise
        else
            1.0 - gammaUpperCont a x // indirectly is faster

    let gammaUpper a x =
      // incomplete Gamma 'Q' (upper) == (1 - GammaP) but computed for efficiency
      // aka 'igamc' (incomplete gamma complement)
        if (x < 0.0 || a <= 0.0) then
            raise (new ArgumentException("Bad args in GammaUpper"))
        if (x < a + 1.0) then
            1.0 - gammaLowerSer a x // indirect is faster
        else
            gammaUpperCont a x

let makeBitArray (bitString:string) =
    bitString.ToCharArray()
    |> Array.filter ((<>) ' ')
    |> Array.map ((=) '1')
    |> (fun a -> new BitArray(a))

let showBitArray (bitArray:BitArray) blockSize lineSize =
    for i = 0 to bitArray.Length-1 do
        if (i > 0 && i % blockSize = 0) then
            printf " "
        if (i > 0 && i % lineSize = 0) then 
            printfn ""

        if (bitArray.Get(i)) then printf "1"
        else printf "0"

let errorFunction (x:float) =
    // assume x > 0.0
    // Abramowitz and Stegun eq. 7.1.26
    let p = 0.3275911
    let a1,a2,a3,a4,a5 = 0.254829592, -0.284496736, 1.421413741, -1.453152027, 1.061405429
    let t = 1.0 / (1.0 + p * x)
    let err = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.Exp(-x * x)
    err

let errorFunctionComplement x = 
    1.0 - errorFunction x

let frequencyTest bitArray = 
    let sum = bitArray
              |> Seq.ofBitArray
              |> Seq.sumBy (fun v -> if v then 1 else -1)
    let testStat = (Math.Abs(float sum)) / (Math.Sqrt(float bitArray.Length));
    let rootTwo = 1.414213562373095;
    let pValue = errorFunctionComplement(testStat / rootTwo);
    pValue

let blockTest (bitArray:BitArray) blockLength =
    let numBlocks = bitArray.Length / blockLength

    let getProportions block =
        let mutable countOnes = 0
        for k=(block*blockLength) to ((block+1)*blockLength)-1 do
            if bitArray.Get(k) then
                countOnes <- countOnes+1
        (float countOnes) / (float blockLength)

    let proportions = Array.init numBlocks getProportions
    let summ = proportions |> Array.sumBy (fun pr -> (pr-0.5)*(pr-0.5))
    let chiSquared = 4.0 * (float blockLength) * summ
    let a = (float numBlocks) / 2.0
    let x = chiSquared / 2.0
    let pValue = GammaFunctions.gammaUpper a x
    pValue

let runsTest (bitArray:BitArray) =
    // NIST Runs test
    let prop = bitArray
               |> Seq.ofBitArray
               |> Seq.sumBy (fun v -> if v then 1.0 else 0.0)
               |> (*) (1.0 / (float bitArray.Length))
    let runs = bitArray
               |> Seq.ofBitArray
               |> Seq.windowed 2
               |> Seq.sumBy (fun e -> if e.[0] = e.[1] then 0.0 else 1.0)
               |> (+) 1.0

    let num = Math.Abs(runs - (2.0 * (float bitArray.Length) * prop * (1.0 - prop)))
    let denom = 2.0 * Math.Sqrt(2.0 * (float bitArray.Length)) * prop * (1.0 - prop)
    let pValue = errorFunctionComplement (num / denom)
    pValue

let run() = 
    printfn "Begin NIST tests of randomness using F# demo\n"
    printfn "Input sequence to test for randomness: \n"
    let bitString = "1100 0011 1010 1110 0000 1111 0000 1111 0000 1111 0000 1111 0000"
    let bitArray = makeBitArray bitString
    showBitArray bitArray 4 52
    
    printfn "\n\n1. Testing input frequencies"
    let pFreq = frequencyTest bitArray
    if (pFreq < 0.01) then
       printfn "There is evidence that sequence is NOT random"
    else
       printfn "Sequence passes NIST frequency test for randomness"

    let blockLength = 8
    printfn "\n\n2. Testing input blocks (block length %i)" blockLength
    let pBlock = blockTest bitArray blockLength
    
    printfn "pValue for Block test = %s" (pBlock.ToString("F4"))
    if (pBlock < 0.01) then
        printfn "There is evidence that sequence is NOT random"
    else
        printfn "Sequence passes NIST block test for randomness"

    printfn "\n\n3. Testing input runs"
    let pRuns = runsTest bitArray
    printfn "pValue for Runs test = %s" (pRuns.ToString("F4"))
    if (pRuns < 0.01) then
        printfn "There is evidence that sequence is NOT random"
    else
        printfn "Sequence passes NIST runs test for randomness"

    printfn "\n\nEnd NIST randomness demo\n"

    1 |> ignore

