using System;
using System.IO;

// Demo logistic regression using Newton-Raphson
// Note: I left in, but commenteed out, a lot of code in that I used for debugging.

namespace LogisticRegressionNewtonRaphson
{
  public class LogisticRegressionNRProgram
  {
    public static void Main(string[] args)
    {
      try
      {
        Console.WriteLine("\nBegin Logistic Regression with Newton-Raphson demo");

        string trainFile = "trainFile.txt";  // will be placed in same directory as executable
        string testFile = "testFile.txt";

        Console.WriteLine("\nCreating 80 lines of synthetic training data and 20 lines of test data");
        MakeRawDataFile(80, 3, trainFile);
        MakeRawDataFile(20, 4, testFile);

        Console.WriteLine("\nFirst 5 lines of training data file are: \n");
        DisplayRawData(trainFile, 5);

        Console.WriteLine("\nLoading train and test data from files into memory");

        double[][] xTrainMatrix = LoadRawDataIntoDesignMatrix(trainFile);
        Console.WriteLine("\nFirst five rows of x training matrix:");
        Console.WriteLine(MatrixAsString(xTrainMatrix, 5, 1, 5));

        double[] yTrainVector = LoadRawDataIntoYVector(trainFile);
        Console.WriteLine("\nFirst five rows of y training vector:");
        Console.WriteLine(VectorAsString(yTrainVector, 5, 1, 3));

        double[][] xTestMatrix = LoadRawDataIntoDesignMatrix(testFile);
        double[] yTestVector = LoadRawDataIntoYVector(testFile);

        Console.WriteLine("Setting Newton-Raphson algorithm stop conditions");
        int maxIterations = 25;
        double epsilon = 0.01; // stop if all new beta values change less than epsilon (algorithm has converged?)
        double jumpFactor = 1000.0; // stop if any new beta jumps too much (algorithm spinning out of control?)
        Console.WriteLine("\nmaxIterations = " + maxIterations + "  epsilon = " + epsilon + "  jumpFactor = " + jumpFactor);

        Console.WriteLine("\nUsing Newton-Raphson to find beta parameters that best fit the training data");
        double[] beta = ComputeBestBeta(xTrainMatrix, yTrainVector, maxIterations, epsilon, jumpFactor); // computing the beta parameters is synonymous with 'training'
        Console.WriteLine("\nNewton-Raphson complete");
        Console.WriteLine("\nThe beta vector is: ");
        Console.WriteLine(VectorAsString(beta, int.MaxValue, 4, 10));

        Console.WriteLine("Computing accuracy on test data using the beta values");
        double acc = PredictiveAccuracy(xTestMatrix, yTestVector, beta); // percent of data cases correctly predicted in the test data set.
        Console.WriteLine("\nThe predictive accuracy of the model on the test data is " + acc.ToString("F2") + "%\n");

        Console.WriteLine("\nEnd demo");
        Console.ReadLine();
      }
      catch (Exception ex)
      {
        Console.WriteLine("Fatal: " + ex.Message);
        Console.ReadLine();
      }

    } // Main

    static void MakeRawDataFile(int numLines, int seed, string fileName)
    {
      FileStream ofs = new FileStream(fileName, FileMode.Create);
      StreamWriter sw = new StreamWriter(ofs);

      double[] bValues = new double[] { -95.0, 0.4, -0.9, 11.2 }; // hard-coded

      Random rand = new Random(seed);

      for (int i = 0; i < numLines; ++i)
      {
        int age = rand.Next(35, 81); // [35,81) == [35,80]
        string sex = (rand.NextDouble() < 0.50) ? "M" : "F";
        double chol = age / 10.0 - 1.0;

        int flip = rand.Next(2);
        if (flip == 0)
          chol = chol + (3.0 - 1.0) * rand.NextDouble() + 1.0;
        else
          chol = chol - (3.0 - 1.0) * rand.NextDouble() + 1.0;
        if (chol > 9.9) chol = 9.9;
        if (chol < 0.1) chol = 0.1;

        double x0 = 1.0;
        double x1 = age;
        double x2 = (sex == "M") ? 0.0 : 1.0;
        double x3 = chol;

        double z = (bValues[0] * x0) + (bValues[1] * x1) + (bValues[2] * x2) + (bValues[3] * x3);
        double p = 1.0 / (1.0 + Math.Exp(-z));

        int die = (p < 0.5) ? 0 : 1;

        sw.WriteLine(age + " " + sex + " " + chol.ToString("F2") + " " + die);
      }
      sw.Close(); ofs.Close();
    }

    static void DisplayRawData(string fileName, int numLines)
    {
      FileStream ifs = new FileStream(fileName, FileMode.Open);
      StreamReader sr = new StreamReader(ifs);

      string line = "";
      string[] tokens = null;
      int ct = 0;

      Console.WriteLine("Age  Sex Chol  |  Died");
      Console.WriteLine("======================");
      while ((line = sr.ReadLine()) != null && ct < numLines)
      {
        tokens = line.Split(' ');
        Console.WriteLine(" " + tokens[0] + "   " + tokens[1] + "  " + tokens[2] + "  |   " + tokens[3]);
        ++ct;
      }
      sr.Close(); ifs.Close();
      Console.WriteLine(" . . .");
    }

    // ============================================================================================

    static double[][] LoadRawDataIntoDesignMatrix(string rawDataFile)
    {
      // read a file of raw age-sex-cholesterol-death (e.g., "68 M 7.56 1") and
      // add an initial column of all 1.0s to correspond to the B0 constant
      // the dependent Y variable (death) is loaded by a separate routine

      FileStream ifs = new FileStream(rawDataFile, FileMode.Open);
      StreamReader sr = new StreamReader(ifs);
      string line = "";
      string[] tokens = null;
      int ct = 0;
      while ((line = sr.ReadLine()) != null) // count number lines in file
        ++ct;
      sr.Close(); ifs.Close();

      double[][] result = MatrixCreate(ct, 4); // design col, age, sex, cholesterol
      ifs = new FileStream(rawDataFile, FileMode.Open);
      sr = new StreamReader(ifs);
      double age;
      double sex;
      double chol;
      int row = 0;

      while ((line = sr.ReadLine()) != null)
      {
        line = line.Trim();
        tokens = line.Split(' ');
        age = (double)(int.Parse(tokens[0]));
        if (tokens[1] == "M")
          sex = 0.0;
        else
          sex = 1.0;
        chol = double.Parse(tokens[2]);

        result[row][0] = 1.0;
        result[row][1] = age;
        result[row][2] = sex;
        result[row][3] = chol;

        ++row;
      }
      sr.Close(); ifs.Close();
      return result;
    } // LoadRawDataIntoDesignMatrix

    static double[] LoadRawDataIntoYVector(string rawDataFile)
    {
      // read a file of raw age-sex-cholesterol-death (68 M 7.56 1) into a y column vector (1.0 0.0 0.0 1.0 etc)
      FileStream ifs = new FileStream(rawDataFile, FileMode.Open);
      StreamReader sr = new StreamReader(ifs);
      string line = "";
      string[] tokens = null;
      int ct = 0;
      while ((line = sr.ReadLine()) != null) // count number lines in file
        ++ct;
      sr.Close(); ifs.Close();

      double[] result = VectorCreate(ct); // single column vector
      ifs = new FileStream(rawDataFile, FileMode.Open);
      sr = new StreamReader(ifs);
      double died;
      int row = 0;

      while ((line = sr.ReadLine()) != null)
      {
        line = line.Trim();
        tokens = line.Split(' ');

        if (int.Parse(tokens[3]) == 1)
          died = 1.0;
        else
          died = 0.0;

        result[row] = died;

        ++row;
      }
      sr.Close(); ifs.Close();
      return result;
    } // LoadRawDataIntoYVector

    static double PredictiveAccuracy(double[][] xMatrix, double[] yVector, double[] bVector)
    {
      // returns the percent (as 0.00 to 100.00) accuracy of the bVector measured by how many lines of data are correctly predicted.
      // note: this is not the same as accuracy as measured by sum of squared deviations between 
      // the probabilities produceed by bVector and 0.0 and 1.0 data in yVector
      // For predictions we simply see if the p produced by b are >= 0.50 or not.

      int xRows = xMatrix.Length; int xCols = xMatrix[0].Length;
      int yRows = yVector.Length;
      int bRows = bVector.Length;
      if (xCols != bRows || xRows != yRows)
        throw new Exception("Bad dimensions for xMatrix or yVector or bVector in PredictiveAccuracy()");

      int numberCasesCorrect = 0;
      int numberCasesWrong = 0;
      double[] pVector = ConstructProbVector(xMatrix, bVector); // helper also used by LogisticRegressionNewtonParameters()
      int pRows = pVector.Length;
      if (pRows != xRows)
        throw new Exception("Unequal rows in prob vector and design matrix in PredictiveAccuracy()");

      for (int i = 0; i < yRows; ++i) // each dependent variable
      {
        if (pVector[i] >= 0.50 && yVector[i] == 1.0)
          ++numberCasesCorrect;
        else if (pVector[i] < 0.50 && yVector[i] == 0.0)
          ++numberCasesCorrect;
        else
          ++numberCasesWrong;
      }

      int total = numberCasesCorrect + numberCasesWrong;
      if (total == 0)
        return 0.0;
      else
        return (100.0 * numberCasesCorrect) / total;
    } // PredictiveAccuracy

    // ============================================================================================

    static double[] ComputeBestBeta(double[][] xMatrix, double[] yVector, int maxIterations, double epsilon, double jumpFactor)
    {
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

      int xRows = xMatrix.Length;
      int xCols = xMatrix[0].Length;

      if (xRows != yVector.Length)
        throw new Exception("The xMatrix and yVector are not compatible in LogisticRegressionNewtonParameters()");

      // initial beta values
      double[] bVector = new double[xCols];
      for (int i = 0; i < xCols; ++i) { bVector[i] = 0.0; } // initialize to 0.0. TODO: consider alternatives
      //Console.WriteLine("The initial B vector is");
      //Console.WriteLine(VectorAsString(bVector)); Console.WriteLine("\n");

      // best beta values found so far
      double[] bestBvector = VectorDuplicate(bVector);

      double[] pVector = ConstructProbVector(xMatrix, bVector); // a column vector of the probabilities of each row using the b[i] values and the x[i] values.
      //Console.WriteLine("The initial Prob vector is: ");
      //Console.WriteLine(VectorAsString(pVector)); Console.WriteLine("\n");

      //double[][] wMatrix = ConstructWeightMatrix(pVector); // deprecated. not needed if we use a shortct to comput WX. See ComputeXtilde.
      //Console.WriteLine("The initial Weight matrix is: ");
      //Console.WriteLine(MatrixAsString(wMatrix)); Console.WriteLine("\n");

      double mse = MeanSquaredError(pVector, yVector);
      int timesWorse = 0; // how many times are the new betas worse (i.e., give worse MSE) than the current betas

      for (int i = 0; i < maxIterations; ++i)
      {
        //Console.WriteLine("=================================");
        //Console.WriteLine(i);

        double[] newBvector = ConstructNewBetaVector(bVector, xMatrix, yVector, pVector); // generate new beta values using Newton-Raphson. could return null.
        if (newBvector == null)
        {
          //Console.WriteLine("The ConstructNewBetaVector() helper method in LogisticRegressionNewtonParameters() returned null");
          //Console.WriteLine("because the MatrixInverse() helper method in ConstructNewBetaVector returned null");
          //Console.WriteLine("because the current (X'X~) product could not be inverted");
          //Console.WriteLine("Returning best beta vector found");
          //Console.ReadLine();
          return bestBvector;
        }

        //Console.WriteLine("New b vector is ");
        //Console.WriteLine(VectorAsString(newBvector)); Console.WriteLine("\n");

        // no significant change?
        if (NoChange(bVector, newBvector, epsilon) == true) // we are done because of no significant change in beta[]
        {
          //Console.WriteLine("No significant change between old beta values and new beta values -- stopping");
          //Console.ReadLine();
          return bestBvector;
        }
        // spinning out of control?
        if (OutOfControl(bVector, newBvector, jumpFactor) == true) // any new beta more than jumpFactor times greater than old?
        {
          //Console.WriteLine("The new beta vector has at least one value which changed by a factor of " + jumpFactor + " -- stopping");
          //Console.ReadLine();
          return bestBvector;
        }

        pVector = ConstructProbVector(xMatrix, newBvector);

        // are we getting worse or better?
        double newMSE = MeanSquaredError(pVector, yVector); // smaller is better
        if (newMSE > mse) // new MSE is worse than current SSD
        {
          ++timesWorse;           // update counter
          if (timesWorse >= 4)
          {
            //Console.WriteLine("The new beta vector produced worse predictions even after modification four times in a row -- stopping");
            return bestBvector;
          }
          //Console.WriteLine("The new beta vector has produced probabilities which give worse predictions -- modifying new betas to halfway between old and new");
          //Console.WriteLine("Times worse = " + timesWorse);

          bVector = VectorDuplicate(newBvector);   // update current b: old b becomes not the new b but halfway between new and old
          for (int k = 0; k < bVector.Length; ++k) { bVector[k] = (bVector[k] + newBvector[k]) / 2.0; }
          mse = newMSE;                            // update current SSD (do not update best b because we don't have a new best b)
          //Console.ReadLine();
        }
        else // new SSD is be better than old
        {
          bVector = VectorDuplicate(newBvector);  // update current b: old b becomes new b
          bestBvector = VectorDuplicate(bVector); // update best b
          mse = newMSE;                           // update current MSE
          timesWorse = 0;                         // reset counter
        }

        //double pa = PredictiveAccuracy(xMatrix, yVector, bestBvector); // how many cases are we correctly predicting
        //Console.WriteLine("Predictive accuracy is " + pa.ToString("F4"));

        //Console.WriteLine("=================================");
        //Console.ReadLine();
      } // end main iteration loop

      //Console.WriteLine("Exceeded max iterations -- stopping");
      //Console.ReadLine();
      return bestBvector;

    } // ComputeBestBeta

    // --------------------------------------------------------------------------------------------

    static double[] ConstructNewBetaVector(double[] oldBetaVector, double[][] xMatrix, double[] yVector, double[] oldProbVector)
    {
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

      double[][] Xt = MatrixTranspose(xMatrix);                 // X'
      double[][] A = ComputeXtilde(oldProbVector, xMatrix);     // WX
      double[][] B = MatrixProduct(Xt, A);                      // X'WX

      double[][] C = MatrixInverse(B);                          // inv(X'WX)
      if (C == null)                                            // computing the inverse can blow up easily
        return null;

      double[][] D = MatrixProduct(C, Xt);                      // inv(X'WX)X'
      double[] YP = VectorSubtraction(yVector, oldProbVector);  // y-p
      double[] E = MatrixVectorProduct(D, YP);                  // inv(X'WX)X'(y-p)
      double[] result = VectorAddition(oldBetaVector, E);       // b + inv(X'WX)X'(y-p)

      return result;                                            // could be null!
    } // ConstructNewBvector

    // --------------------------------------------------------------------------------------------

    static double[][] ComputeXtilde(double[] pVector, double[][] xMatrix)
    {
      // note: W[t-1] is nxn which could be huge so instead of computing b[t] = b[t-1] + inv(X'W[t-1]X)X'(y - p[t-1]) directly
      // we compute the W[t-1]X part, without the use of W.
      // Since W is derived from the prob vector and W has non-0.0 elements only on the diagonal we can avoid a ton of work
      // by using the prob vector directly and not computing W at all.
      // Some of the research papers refer to the product W[t-1]X as X~ hence the name of this method.
      // ex: if xMatrix is 10x4 then W would be 10x10 so WX would be 10x4 -- the same size as X

      int pRows = pVector.Length;
      int xRows = xMatrix.Length;
      int xCols = xMatrix[0].Length;

      if (pRows != xRows)
        throw new Exception("The pVector and xMatrix are not compatible in ComputeXtilde");

      // we are not doing marix multiplication. the p column vector sort of lays on top of each matrix column.
      double[][] result = MatrixCreate(pRows, xCols); // could use (xRows, xCols) here

      for (int i = 0; i < pRows; ++i)
      {
        for (int j = 0; j < xCols; ++j)
        {
          result[i][j] = pVector[i] * (1.0 - pVector[i]) * xMatrix[i][j]; // note the p(1-p)
        }
      } // i
      return result;
    } // ComputeXtilde


    // --------------------------------------------------------------------------------------------

    static bool NoChange(double[] oldBvector, double[] newBvector, double epsilon)
    {
      // true if all new b values have changed by amount smaller than epsilon
      for (int i = 0; i < oldBvector.Length; ++i)
      {
        if (Math.Abs(oldBvector[i] - newBvector[i]) > epsilon) // we have at least one change
          return false;
      }
      return true;
    } // NoChange

    static bool OutOfControl(double[] oldBvector, double[] newBvector, double jumpFactor)
    {
      // true if any new b is jumpFactor times greater than old b
      for (int i = 0; i < oldBvector.Length; ++i)
      {
        if (oldBvector[i] == 0.0) return false; // if old is 0.0 anything goes for the new value

        if (Math.Abs(oldBvector[i] - newBvector[i]) / Math.Abs(oldBvector[i]) > jumpFactor) // too big a change.
          return true;
      }
      return false;
    }

    // --------------------------------------------------------------------------------------------

    static double[] ConstructProbVector(double[][] xMatrix, double[] bVector)
    {
      // p = 1 / (1 + exp(-z) where z = b0x0 + b1x1 + b2x2 + b3x3 + . . .
      // suppose X is 10 x 4 (cols are: x0 = const. 1.0, x1, x2, x3)
      // then b would be a 4 x 1 (col vecror)
      // then result of X times b is (10x4)(4x1) = (10x1) column vector

      int xRows = xMatrix.Length;
      int xCols = xMatrix[0].Length;
      int bRows = bVector.Length;

      if (xCols != bRows)
        throw new Exception("xMatrix and bVector are not compatible in ConstructProbVector");

      double[] result = VectorCreate(xRows); // ex: if xMatrix is size 10 x 4 and bVector is 4 x 1 then prob vector is 10 x 1 (one prob for every row of xMatrix)

      double z = 0.0;
      double p = 0.0;

      for (int i = 0; i < xRows; ++i)
      {
        z = 0.0;
        for (int j = 0; j < xCols; ++j)
        {
          z += xMatrix[i][j] * bVector[j]; // b0(1.0) + b1x1 + b2x2 + . . .
        }
        p = 1.0 / (1.0 + Math.Exp(-z));  // consider checking for huge value of Math.Exp(-z) here
        result[i] = p;
      }
      return result;
    } // ConstructProbVector

    // --------------------------------------------------------------------------------------------

    static double MeanSquaredError(double[] pVector, double[] yVector)
    {
      // how good are the predictions? (using an already-calculated prob vector)
      // note: it is possible that a model with better (lower) MSE than a second model could give worse predictive accuracy.
      int pRows = pVector.Length;
      int yRows = yVector.Length;
      if (pRows != yRows)
        throw new Exception("The prob vector and the y vector are not compatible in MeanSquaredError()");
      if (pRows == 0)
        return 0.0;
      double result = 0.0;
      for (int i = 0; i < pRows; ++i)
      {
        result += (pVector[i] - yVector[i]) * (pVector[i] - yVector[i]);
        //result += Math.Abs(pVector[i] - yVector[i]); // average absolute deviation approach
      }
      return result / pRows;
    }

    // --------------------------------------------------------------------------------------------

    // ============================================================================================

    static double[][] MatrixCreate(int rows, int cols)
    {
      // creates a matrix initialized to all 0.0. assume rows and cols > 0
      double[][] result = new double[rows][];
      for (int i = 0; i < rows; ++i) { result[i] = new double[cols]; } // explicit initialization not necessary.
      return result;
    }

    static double[] VectorCreate(int rows) // all vectors in Newton-Raphson are single-column vectors
    {
      double[] result = new double[rows]; // we use this technique when we want to make column vector creation explicit
      return result;
    }

    static string MatrixAsString(double[][] matrix, int numRows, int digits, int width)
    {
      string s = "";
      for (int i = 0; i < matrix.Length && i < numRows; ++i)
      {
        for (int j = 0; j < matrix[i].Length; ++j)
        {
          s += matrix[i][j].ToString("F"+digits).PadLeft(width) + " ";
        }
        s += Environment.NewLine;
      }
      return s;
    } // MatrixAsString

    static double[][] MatrixDuplicate(double[][] matrix)
    {
      // allocates/creates a duplicate of a matrix. assumes matrix is not null.
      double[][] result = MatrixCreate(matrix.Length, matrix[0].Length);
      for (int i = 0; i < matrix.Length; ++i) // copy the values
        for (int j = 0; j < matrix[i].Length; ++j)
          result[i][j] = matrix[i][j];
      return result;
    }

    static double[] VectorAddition(double[] vectorA, double[] vectorB)
    {
      int aRows = vectorA.Length;
      int bRows = vectorB.Length;
      if (aRows != bRows)
        throw new Exception("Non-conformable vectors in VectorAddition");
      double[] result = new double[aRows];
      for (int i = 0; i < aRows; ++i)
        result[i] = vectorA[i] + vectorB[i];
      return result;
    }

    static double[] VectorSubtraction(double[] vectorA, double[] vectorB)
    {
      int aRows = vectorA.Length;
      int bRows = vectorB.Length;
      if (aRows != bRows)
        throw new Exception("Non-conformable vectors in VectorSubtraction");
      double[] result = new double[aRows];
      for (int i = 0; i < aRows; ++i)
        result[i] = vectorA[i] - vectorB[i];
      return result;
    }

    static string VectorAsString(double[] vector, int count, int digits, int width)
    {
      string s = "";
      for (int i = 0; i < vector.Length && i < count; ++i)
        s += " " + vector[i].ToString("F"+digits).PadLeft(width) + Environment.NewLine;
      s += Environment.NewLine;
      return s;
    }

    static double[] VectorDuplicate(double[] vector)
    {
      double[] result = new double[vector.Length];
      for (int i = 0; i < vector.Length; ++i)
        result[i] = vector[i];
      return result;
    }

    static double[][] MatrixTranspose(double[][] matrix) // assumes matrix is not null
    {
      int rows = matrix.Length;
      int cols = matrix[0].Length; // assume all columns have equal size
      double[][] result = MatrixCreate(cols, rows); // note the indexing swap
      for (int i = 0; i < rows; ++i)
      {
        for (int j = 0; j < cols; ++j)
        {
          result[j][i] = matrix[i][j];
        }
      }
      return result;
    } // TransposeMatrix

    static double[][] MatrixProduct(double[][] matrixA, double[][] matrixB)
    {
      int aRows = matrixA.Length; int aCols = matrixA[0].Length;
      int bRows = matrixB.Length; int bCols = matrixB[0].Length;
      if (aCols != bRows)
        throw new Exception("Non-conformable matrices in MatrixProduct");

      double[][] result = MatrixCreate(aRows, bCols);

      for (int i = 0; i < aRows; ++i) // each row of A
        for (int j = 0; j < bCols; ++j) // each col of B
          for (int k = 0; k < aCols; ++k) // could use k < bRows
            result[i][j] += matrixA[i][k] * matrixB[k][j];

      return result;
    } // MatrixProduct

    static double[] MatrixVectorProduct(double[][] matrix, double[] vector)
    {
      int mRows = matrix.Length; int mCols = matrix[0].Length;
      int vRows = vector.Length;
      if (mCols != vRows)
        throw new Exception("Non-conformable matrix and vector in MatrixVectorProduct");
      double[] result = new double[mRows]; // an n x m matrix times a m x 1 column vector is a n x 1 column vector
      for (int i = 0; i < mRows; ++i)
        for (int j = 0; j < mCols; ++j)
          result[i] += matrix[i][j] * vector[j];
      return result;
    }

    static double[][] MatrixInverse(double[][] matrix)
    {
 
      int n = matrix.Length;
      double[][] result = MatrixDuplicate(matrix);

      int[] perm;
      int toggle;
      double[][] lum = MatrixDecompose(matrix, out perm, out toggle);
      if (lum == null)
        return null;

      double[] b = new double[n];
      for (int i = 0; i < n; ++i)
      {
        for (int j = 0; j < n; ++j)
        {
          if (i == perm[j])
            b[j] = 1.0;
          else
            b[j] = 0.0;
        }

        double[] x = HelperSolve(lum, b); // 

        for (int j = 0; j < n; ++j)
          result[j][i] = x[j];
      }
      return result;
    }

    // -------------------------------------------------------------------------------------------------------------------

    static double[] HelperSolve(double[][] luMatrix, double[] b) // helper
    {
      // solve Ax = b if you already have luMatrix from A and b has been permuted
      int n = luMatrix.Length;

      // 1. make a copy of the permuted b vector
      double[] x = new double[n];
      b.CopyTo(x, 0);

      // 2. solve Ly = b using forward substitution
      for (int i = 1; i < n; ++i)
      {
        double sum = x[i];
        for (int j = 0; j < i; ++j)
        {
          sum -= luMatrix[i][j] * x[j];
        }
        x[i] = sum;
      }

      // 3. solve Ux = y using backward substitution
      x[n - 1] /= luMatrix[n - 1][n - 1];
      for (int i = n - 2; i >= 0; --i)
      {
        double sum = x[i];
        for (int j = i + 1; j < n; ++j)
        {
          sum -= luMatrix[i][j] * x[j];
        }
        x[i] = sum / luMatrix[i][i];
      }

      return x;
    } // HelperSolve

    // -------------------------------------------------------------------------------------------------------------------

    static double[][] MatrixDecompose(double[][] matrix, out int[] perm, out int tog)
    {
      // Doolittle's method (1.0s on L diagonal) with partial pivoting
      int rows = matrix.Length;
      int cols = matrix[0].Length; // assume all rows have the same number of columns so just use row [0].
      if (rows != cols)
        throw new Exception("Attempt to MatrixDecompose a non-square mattrix");

      int n = rows; // convenience

      double[][] result = MatrixDuplicate(matrix); // make a copy of the input matrix

      perm = new int[n]; // set up row permutation result
      for (int i = 0; i < n; ++i) { perm[i] = i; }

      tog = 1; // toggle tracks number of row swaps. used by MatrixDeterminant

      double ajj, aij;

      for (int j = 0; j < n - 1; ++j) // each column
      {
        double max = Math.Abs(result[j][j]); // find largest value in row
        int pRow = j;
        for (int i = j + 1; i < n; ++i)
        {
          aij = Math.Abs(result[i][j]);
          if (aij > max)
          {
            max = aij;
            pRow = i;
          }
        }

        if (pRow != j) // if largest value not on pivot, swap rows
        {
          double[] rowPtr = result[pRow];
          result[pRow] = result[j];
          result[j] = rowPtr;

          int tmp = perm[pRow]; // and swap perm info
          perm[pRow] = perm[j];
          perm[j] = tmp;

          tog = -tog; // adjust the row-swap toggle
        }

        ajj = result[j][j];
        if (Math.Abs(ajj) < 0.00000001) // if diagonal after swap is zero . . .
          return null; // consider a throw

        for (int i = j + 1; i < n; ++i)
        {
          aij = result[i][j] / ajj;
          result[i][j] = aij;
          for (int k = j + 1; k < n; ++k)
          {
            result[i][k] -= aij * result[j][k];
          }
        }
      } // main j loop

      return result;
    } // MatrixDecompose

    // ===========================================================================================

  } // LogisticRegressionNRProgram

} // ns
