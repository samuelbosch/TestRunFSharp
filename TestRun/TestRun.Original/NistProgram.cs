using System;
using System.Collections;

namespace NistRandomness
{
  public class NistProgram
  {
    public static void Main(string[] args)
    {
      try
      {
        Console.WriteLine("\nBegin NIST tests of randomness using C# demo\n");

        //double ig = GammaFunctions.GammaUpper(600, 600); // see TS p. E-2. Maple = 0.4945710333, Matlab = 0.4945710331, TS = 0.4945710331
        //Console.WriteLine(ig); // adjust desired accuracy, max-iterations -> C# = 0.4945710332

        //string bitString = "1100 1001 0000 1111 1101 1010 1010 0010 0010 0001 0110 1000 1100 0010 0011 0100 1100 0100 1100 0110 0110 0010 1000 1011 1000"; // page 2-5
        string bitString = "1100 0011 1010 1110 0000 1111 0000 1111 0000 1111 0000 1111 0000";

        BitArray bitArray = MakeBitArray(bitString);
        Console.WriteLine("Input sequence to test for randomness: \n");
        ShowBitArray(bitArray, 4, 52);

        Console.WriteLine("\n\n1. Testing input frequencies");
        double pFreq = FrequencyTest(bitArray);
        Console.WriteLine("pValue for Frequency test = " + pFreq.ToString("F4"));
        if (pFreq < 0.01)
          Console.WriteLine("There is evidence that sequence is NOT random");
        else
          Console.WriteLine("Sequence passes NIST frequency test for randomness");

        int blockLength = 8;
        Console.WriteLine("\n\n2. Testing input blocks (block length = " + blockLength + ")"); 
        double pBlock = BlockTest(bitArray, blockLength);
        Console.WriteLine("pValue for Block test = " + pBlock.ToString("F4"));
        if (pBlock < 0.01)
          Console.WriteLine("There is evidence that sequence is NOT random");
        else
          Console.WriteLine("Sequence passes NIST block test for randomness");

        Console.WriteLine("\n\n3. Testing input runs");
        double pRuns = RunsTest(bitArray);
        Console.WriteLine("pValue for Runs test = " + pRuns.ToString("F4"));
        if (pRuns < 0.01)
          Console.WriteLine("There is evidence that sequence is NOT random");
        else
          Console.WriteLine("Sequence passes NIST runs test for randomness");

        Console.WriteLine("\n\nEnd NIST randomness demo\n");
        Console.ReadLine();
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message);
        Console.ReadLine();
      }
    } // Main

    // ------------------------------------------------------------------

    static BitArray MakeBitArray(string bitString)
    {
      // ex: string "010 101" -> a BitArray of [false,true,false,true,false,true]
      int size = 0;
      for (int i = 0; i < bitString.Length; ++i)
        if (bitString[i] != ' ') ++size;

      BitArray result = new BitArray(size); // default is false
      int k = 0; // ptr into result
      for (int i = 0; i < bitString.Length; ++i)
      {
        if (bitString[i] == ' ') continue;
        if (bitString[i] == '1')
          result[k] = true;
        else
          result[k] = false; // not necessary in C#
        ++k;
      }
      return result;
    }

    static void ShowBitArray(BitArray bitArray, int blockSize, int lineSize)
    {
      for (int i = 0; i < bitArray.Length; ++i)
      {
        if (i > 0 && i % blockSize == 0)
          Console.Write(" ");

        if (i > 0 && i % lineSize == 0)
          Console.WriteLine("");

        if (bitArray[i] == false) Console.Write("0");
        else Console.Write("1");
      }
      Console.WriteLine("");
    }

    // ------------------------------------------------------------------

   
    static double FrequencyTest(BitArray bitArray)
    {
      // perform a NIST frequency test on bitArray
      double sum = 0;
      for (int i = 0; i < bitArray.Length; ++i)
      {
        if (bitArray[i] == false)
          sum = sum - 1;
        else
          sum = sum + 1;
      }
      double testStat = Math.Abs(sum) / Math.Sqrt(bitArray.Length);
      double rootTwo = 1.414213562373095;
      double pValue = ErrorFunctionComplement(testStat / rootTwo);
      return pValue;
    }

    static double ErrorFunction(double x)
    {
      // assume x > 0.0
      // Abramowitz and Stegun eq. 7.1.26
      double p = 0.3275911;
      double a1 = 0.254829592;
      double a2 = -0.284496736;
      double a3 = 1.421413741;
      double a4 = -1.453152027;
      double a5 = 1.061405429;
      double t = 1.0 / (1.0 + p * x);
      double err = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.Exp(-x * x);
      return err;
    }

    static double ErrorFunctionComplement(double x)
    {
      return 1 - ErrorFunction(x);
    }

    // ------------------------------------------------------------------

    static double BlockTest(BitArray bitArray, int blockLength)
    {
      // NIST intra-block frequency test
      int numBlocks = bitArray.Length / blockLength; // 'N'

      double[] proportions = new double[numBlocks];
      int k = 0; // ptr into bitArray
      for (int block = 0; block < numBlocks; ++block)
      {
        int countOnes = 0;
        for (int i = 0; i < blockLength; ++i)
        {
          if (bitArray[k++] == true)
            ++countOnes;
        }

        proportions[block] = (countOnes * 1.0) / blockLength;
      }

      double summ = 0.0;
      for (int block = 0; block < numBlocks; ++block)
        summ = summ + (proportions[block] - 0.5) * (proportions[block] - 0.5);
      double chiSquared = 4 * blockLength * summ;

      double a = numBlocks / 2.0;
      double x = chiSquared / 2.0;
      double pValue = GammaFunctions.GammaUpper(a, x);
      return pValue;
    }

    static double RunsTest(BitArray bitArray)
    {
      // NIST Runs test
      double numOnes = 0.0;
      for (int i = 0; i < bitArray.Length; ++i)
        if (bitArray[i] == true)
          ++numOnes;

      double prop = (numOnes * 1.0) / bitArray.Length;

      //double tau = 2.0 / Math.Sqrt(bitArray.Length * 1.0);
      //if (Math.Abs(prop - 0.5) >= tau)
      //  return 0.0; // not-random short-circuit

      int runs = 1;
      for (int i = 0; i < bitArray.Length - 1; ++i)
        if (bitArray[i] != bitArray[i + 1])
          ++runs;
 
      double num = Math.Abs(runs - (2 * bitArray.Length * prop * (1 - prop)));
      double denom = 2 * Math.Sqrt(2.0 * bitArray.Length) * prop * (1 - prop);
      double pValue = ErrorFunctionComplement(num / denom);
      return pValue;
    }


    // ------------------------------------------------------------------
  } // Program

  public class GammaFunctions
  {
    public static double GammaLower(double a, double x)
    {
      // incomplete Gamma 'P' (lower) aka 'igam'
      if (x < 0.0 || a <= 0.0)
        throw new Exception("Bad args in GammaLower");
      if (x < a + 1)
        return GammaLowerSer(a, x); // no surprise
      else
        return 1.0 - GammaUpperCont(a, x); // indirectly is faster
    }

    public static double GammaUpper(double a, double x)
    {
      // incomplete Gamma 'Q' (upper) == (1 - GammaP) but computed for efficiency
      // aka 'igamc' (incomplete gamma complement)
      if (x < 0.0 || a <= 0.0)
        throw new Exception("Bad args in GammaUpper");
      if (x < a + 1)
        return 1.0 - GammaLowerSer(a, x); // indirect is faster
      else
        return GammaUpperCont(a, x);
    }

    // -------------------------------------------------------------------------------

    private static double LogGamma(double x)
    {
      // Log of Gamma from Lanczos with g=5, n=6/7
      // not in A & S 
      double[] coef = new double[6] { 76.18009172947146, -86.50532032941677, 
        24.01409824083091, -1.231739572450155, 
        0.1208650973866179E-2, -0.5395239384953E-5 }; 
      double LogSqrtTwoPi = 0.91893853320467274178;
      double denom = x + 1;
      double y = x + 5.5;
      double series = 1.000000000190015;
      for (int i = 0; i < 6; ++i)
      {
        series += coef[i] / denom;
        denom += 1.0;
      }
      return (LogSqrtTwoPi + (x + 0.5) * Math.Log(y) - y + Math.Log(series / x));
    }

    private static double GammaLowerSer(double a, double x)
    {
      // incomplete gamma lower (computed by series expansion)
      if ( x < 0.0)
        throw new Exception("x param less than 0.0 in GammaLowerSer");

      double gln = LogGamma(a);
      double ap = a;
      double del = 1.0 / a;
      double sum = del;
      for (int n = 1; n <= 100; ++n)
      {
        ++ap;
        del *= x / ap;
        sum += del;
        if (Math.Abs(del) < Math.Abs(sum) * 3.0E-7) // close enough?
          return sum * Math.Exp(-x + a * Math.Log(x) - gln);
      }
      throw new Exception("Unable to compute GammaLowerSer to desired accuracy");
    }

    private static double GammaUpperCont(double a, double x)
    {
      // incomplete gamma upper computed by continuing fraction
      if (x < 0.0)
        throw new Exception("x param less than 0.0 in GammaUpperCont");
      double gln = LogGamma(a);
      double b = x + 1.0 - a;
      double c = 1.0 / 1.0E-30; // div by close to double.MinValue
      double d = 1.0 / b;
      double h = d;
      for (int i = 1; i <= 100; ++i)
      {
        double an = -i * (i - a);
        b += 2.0;
        d = an * d + b;
        if (Math.Abs(d) < 1.0E-30) d = 1.0E-30; // got too small?
        c = b + an / c;
        if (Math.Abs(c) < 1.0E-30) c = 1.0E-30;
        d = 1.0 / d;
        double del = d * c;
        h *= del;
        if (Math.Abs(del - 1.0) < 3.0E-7)
          return Math.Exp(-x + a * Math.Log(x) - gln) * h;  // close enough?
      }
      throw new Exception("Unable to compute GammaUpperCont to desired accuracy");
    }

    // -------------------------------------------------------------------------------

    //public static double LogGamma(double z)
    //{
    //  // alternate, not quite as accurate, LogGamma()
    //  // A & S 6.1.41 (Stirling's approximation)
    //  double x1 = (z - 0.5) * Math.Log(z);
    //  double x3 = 0.5 * Math.Log(2 * Math.PI);
    //  double x4 = 1 / (12 * z);
    //  double x5 = 1 / (360 * z * z * z);
    //  double x6 = 1 / (1260 * z * z * z * z * z);
    //  double x7 = 1 / (1680 * z * z * z * z * z * z * z);
    //  return x1 - z + x3 + x4 - x5 + x6 - x7;
    //}

  } // GammaFunctions

} // ns
