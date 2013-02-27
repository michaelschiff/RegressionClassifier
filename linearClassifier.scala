import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._
import scala.collection.mutable.ArrayBuffer
import scala.math
import scala.util.Random
  
trainAndTest.main(Array())

object trainAndTest {
  def main(args: Array[String]) {
    println("loading data")
    //build buffer of X blocks
    val xList: ArrayBuffer[SMat] = new ArrayBuffer()
    for ( i <- 1 to 975 ) { 
      val block: SMat = load("CountsOut/CountsStemmedX"+i+".mat", i+"CountsStemmedX")
      xList += block
    }
    //build buffer of Y blocks
    var y: IMat = load("CountsOut/CountsY.mat", "CountsY")
    y = y.t
    val yList: ArrayBuffer[FMat] = new ArrayBuffer()
    for (i <- 0 to y.ncols by 1000) {
      if (i+999 < y.ncols ) { yList += FMat(y(?, i to i+999).t) }
    }
    //initialize classifier
    println("creating and training classifier")
    val classifier = new trainer(xList, yList, 0.00000001)
  }
}

class trainer(XList: ArrayBuffer[SMat], YList: ArrayBuffer[FMat], a: Double) {
  //basic error checking
  println("checking that num examples matches num labels")
  if ( XList.length != YList.length ) { println("ERROR: num examples does not match num labels") }
  var numWeights = XList(0).nrows
  println("checking that all example blocks have same nrows")
  for ( X <- XList ) { if ( X.nrows != numWeights ) { println("ERROR: all X blocks do not have same nrows") } }
  
  //step size
  var ALPHA: Double = a
  //column vector of weights
  var w: FMat = zeros(numWeights ,1)

  // variables to store hold out data
  var XTest: ArrayBuffer[SMat] = new ArrayBuffer[SMat]()
  var YTest: ArrayBuffer[FMat] = new ArrayBuffer[FMat]()
  
  //function to calculate gradients for each weight given a block of X and Y from the training sets
  def gradients(X: SMat, Y:FMat): FMat = {
    if ( X.ncols != Y.nrows ) { println("ERROR: block dimensions to not match") }
    val combo = X Tmult(w, null) //X is sparse w is a COLUMN!!!
    val diff = combo - Y
    val twice_diff = diff * 2.0f
    var gs = X SMult(sparse(twice_diff), null)
    gs = gs /@ X.ncols
    // DO RIDGE REGULARIZATION HERE
    return gs
  }

  //function to calculate performance given a block of X and Y from the test set
  def error(X: SMat, Y:FMat): Float = sum(abs(X.Tmult(w, null) - Y), 1)(0,0)
  
  //training loop
  var iters = 1
  while (true) { //currently train forever, we are evaluating performance as we go
    //partition XList into XTrain XTest and YList into YTrain YTest
    val rng = new Random()
    for ( i <- 0 to 97 ) { //randomly select 98 matching blocks of X and Y to use as hold out
      val blockNum = rng.nextInt(XList.size)
      val xBlock = XList.remove(blockNum)
      val yBlock = YList.remove(blockNum)
      XTest += xBlock
      YTest += yBlock
    }
    //zip training X/Y and test X/Y
    val trainingExamples = XList.zip(YList)
    val testingExamples = XTest.zip(YTest)
    //make an adjustment to the weights for every trainingExample
    for ( (e,l) <- trainingExamples ) {
      w -= gradients(e,l) * ALPHA
    }
    //calculate absolute error for all testingExamples
    var err: Float = 0.0f
    for ( (e,l) <- testingExamples ) {
      err += error(e, l)
    }
    println("Iteration " + iters + ".  Absolute error: " + err)

    //put the current test data back into the main list
    //this effectively shuffles over multiple iterations while keeping
    //corresponding blocks of X and Y together
    for ( (tx,ty) <- XTest.zip(YTest) ) {
      XList += tx
      YList += ty
    }
    iters += 1
  }
}
