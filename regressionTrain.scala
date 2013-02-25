import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._
import scala.collection.mutable.ListBuffer

lineTest.main(Array())
trainAndTest.main(Array())

  // X : a matrix of examples.  Each column is an example, each row is a feature
  // Y : a column vector of labels.  ith row is the label for the ith col of X
  // a : the step size
  // t : how much error we are willing to accept
  class trainer(XList: ListBuffer[FMat], YList: ListBuffer[FMat], a: Float, t: Float) {
    if ( XList.length != YList.length ) { println("ERROR: num examples does not match num labels") }
    var numWeights = XList(0).nrows
    for ( X <- XList ) { if ( X.nrows != numWeights ) { println("ERROR: all X blocks do not have same nrows") } }
    val THRESHOLD: Float = t
    var ALPHA: Float = a
    var w: FMat = zeros(numWeights ,1)
   
    def gradients(X: FMat, Y:FMat): FMat = {
      if ( X.ncols != Y.nrows ) { println("ERROR: block dimensions to not match") }
      val combo = (w.t * X).t
      val diff = combo - Y
      val twice_diff = diff * 2.0f
      var gs = X * twice_diff
      gs = gs /@ X.ncols
      return gs
    }

    def error(X: FMat, Y:FMat): FMat = {
      val k: FMat = gradients(X, Y)
      val e: Float = abs(k)
      return e
    }
    
    def predict(x: FMat): Float = (x*w)(0,0)
    
    //setup for training loop
    val examples = XList.zip(YList)
    var iters = 0
    var err: FMat =  0.0f
    for ( (e,l) <- examples ) {
      err += error(e, l)
    }
    var errScore: Float = max(err, 1)

    //training loop
    while ( errScore > THRESHOLD ) {
      for ( (e,l) <- examples ) {
        w -= gradients(e, l) * ALPHA
      }
      iters += 1
      err = zeros(err.ncols, 1)
      for ( (e,l)  <- examples ) {
        err += error(e, l)
      }
      errScore = max(err,1)
      //if ( iters%20 == 0 ) { ALPHA = ALPHA * 0.9f }
    }
  }

  object lineTest {
    def main(args: Array[String]) {
      val X:FMat = (1 \ 2 \ 3) on (1 \ 1 \ 1) on (1 \ 1 \ 1)
      val Y:FMat = 1 on 2 on 3
      println("X:\n" + X + "\nY:\n" + Y)
      val classifier = new trainer(new ListBuffer(X), new ListBuffer(Y), 0.001f, 0.00000000001f)
      println("Learned weights:\n" + classifier.w)
      println("Weights should be:\n" + X\\Y)
    }
  }

  object trainAndTest {
    def main(args: Array[String]) {
      println("loading data")
      val xList = new ListBuffer()
      for ( i <- 1 to 975 ) { 
        val block: SMat = load("out/TrimmedSparse"+i+".mat", i+"X")
        xList +=: block
      }
      val lastBlock: SMat = load("out/TrimmedSparseLastX.mat", "LastX")
      xList +=: lastBlock

      var y = load("out/TrimmedSparseY")
      y = y.t
      val yList = new ListBuffer()
      for (i <- 0 to y.ncols by 1000) {
        if (i+999 < y.ncols ) { yList +=: y(?, i to i+999).t }
        else { yList +=: y(?, i to y.ncols-1).t }
      }
      

      println("creating and training classifier")
      val classifier = new trainer(xList, yList, 0.001f, 0.000001f)
      for ( i <- 0 to x.ncols-1 ) { 
        println("classifier predicted: " + classifier.predict(full(x(?, i)).t) )
        println("actually was: " + y(0, i))
      }
    }
  }
