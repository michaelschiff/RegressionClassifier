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
  class trainer(XList: ListBuffer[SMat], YList: ListBuffer[FMat], a: Float, t: Float) {
    println("checking that num examples matches num labels")
    if ( XList.length != YList.length ) { println("ERROR: num examples does not match num labels") }
    var numWeights = XList(0).nrows
    println("checking that all example blocks have same nrows")
    for ( X <- XList ) { if ( X.nrows != numWeights ) { println("ERROR: all X blocks do not have same nrows") } }
    val THRESHOLD: Float = t
    var ALPHA: Float = a
    var w: FMat = zeros(numWeights ,1)
   
    def gradients(X: SMat, Y:FMat): FMat = {
      if ( X.ncols != Y.nrows ) { println("ERROR: block dimensions to not match") }
      //val combo = (w.t * X).t
      val combo = X Tmult(w, null) //X is sparse w is a COLUMN!!!
      val diff = combo - Y
      val twice_diff = diff * 2.0f
      //var gs = X * twice_diff
      var gs = X SMult(sparse(twice_diff), null)
      gs = gs /@ X.ncols
      return gs
    }

    def error(X: SMat, Y:FMat): FMat = {
      val k: FMat = gradients(X, Y)
      val e: FMat = abs(k)
      return e
    }
    
    def predict(x: FMat): Float = (x*w)(0,0)
    
    //setup for training loop
    println("zipping examples")
    val examples = XList.zip(YList)
    var iters = 0
    var err: FMat = zeros(numWeights, 1)
    println("calculating initial err")
    for ( (e,l) <- examples ) {
      err += error(e, l)
    }
    var errScore: Float = maxi(err, 2)(0,0)
    var oldErrScore = errScore + 1
    //training loop
    println("off to the races")
    while ( errScore > THRESHOLD ) {
      err = zeros(numWeights, 1)
      for ( (e,l) <- examples ) {
        val gs = gradients(e,l)
        w -= gs * ALPHA
        err += abs(gs)
      }
      iters += 1
      oldErrScore = errScore
      errScore = maxi(err,2)(0,0)
      if ( true ) { 
        println("Trained " + iters + "iterations")
        println(errScore*10000)
      }
    }
  }

  object lineTest {
    def main(args: Array[String]) {
      val X:FMat = (1 \ 2 \ 3) on (1 \ 1 \ 1) on (1 \ 1 \ 1)
      val Y:FMat = 1 on 2 on 3
      println("X:\n" + X + "\nY:\n" + Y)
      val classifier = new trainer(new ListBuffer() += sparse(X), new ListBuffer() += Y, 0.001f, 0.0001f)
      println("Learned weights:\n" + classifier.w)
      println("Weights should be:\n" + X\\Y)
    }
  }

  object trainAndTest {
    def main(args: Array[String]) {
      println("loading data")
      val xList: ListBuffer[SMat] = new ListBuffer()
      for ( i <- 1 to 975 ) { 
        val block: SMat = load("CountsOut/CountsStemmedX"+i+".mat", i+"CountsStemmedX")
        xList += block
      }
      //val lastBlock: SMat = load("CountsOut/CountsStemmedLastX.mat", "LastCountsStemmedX")
      //xList += lastBlock

      var y: IMat = load("CountsOut/CountsY.mat", "CountsY")
      y = y.t
      val yList: ListBuffer[FMat] = new ListBuffer()
      for (i <- 0 to y.ncols by 1000) {
        if (i+999 < y.ncols ) { yList += FMat(y(?, i to i+999).t) }
        //else { yList += FMat(y(?, i to y.ncols-1).t) }
      }
      

      println("creating and training classifier")
      val classifier = new trainer(xList, yList, 0.0000001f, 0.001f)
      
      var testX = full(xList(0))
      var testY = yList(0)
      for (i <- 0 to testX.ncols) {
        println("classifier predicted: " + classifier.predict(testX(?, i).t))
        println("actually was: " + testY(i,0))
      }
    }
  }
