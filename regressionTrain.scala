import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

lineTest.main(Array())
trainAndTest.main(Array())

  // X : a matrix of examples.  Each column is an example, each row is a feature
  // Y : a column vector of labels.  ith row is the label for the ith col of X
  // a : the step size
  // t : how much error we are willing to accept
  class trainer(X: FMat, Y: FMat, a: Float, t: Float) {
    if ( X.ncols != Y.nrows ) { println("ERROR: num examples does not match num labels") }
    Mat.noMKL=true
    val THRESHOLD: Float = t
    var ALPHA: Float = a
    var w: FMat = zeros(X.nrows ,1)
    def gradients(): FMat = {
      val combo = (w.t * X).t
      val diff = combo - Y
      val twice_diff = (diff * 2.0f)
      var gs = X * twice_diff
      gs = gs /@ X.ncols
      //println("gradient 0:\n" + gs(0,0))
      return gs
    }
    def error(): Float = {
      val k: FMat = gradients()
      val e: Float = sum(abs(k), 1)(0,0)
      println(e)
      return e
    }
    def predict(x: FMat): Float = (x*w)(0,0)
    var iters = 0
    var oldErr: Float = 10000000.0f
    var err: Float = error()
    while ( oldErr - err > THRESHOLD ) {
      w -= gradients() * ALPHA
      iters += 1
      oldErr = err
      err = error()
      //if ( iters%20 == 0 ) { ALPHA = ALPHA * 0.9f }
    }
  }

  object lineTest {
    def main(args: Array[String]) {
      val X:FMat = (1 \ 2 \ 3) on (1 \ 1 \ 1) on (1 \ 1 \ 1)
      val Y:FMat = 1 on 2 on 3
      println("X:\n" + X + "\nY:\n" + Y)
      val classifier = new trainer(X, Y, 0.001f, 0.00000000001f)
      println("Learned weights:\n" + classifier.w)
      println("Weights should be:\n" + X\\Y)
    }
  }

  object trainAndTest {
    def main(args: Array[String]) {
      println("loading data")
      val e: SMat = load("TrimmedSparse.mat", "X")
      var l: IMat = load("TrimmedSparse.mat", "Y")
      l = l(0 to 10000, 0)
      print("training classifier")
      val classifier = new trainer(full(e), FMat(l), 0.01f, 0.0001f)
      println("finished training")
      for ( i <- 0 to e.ncols-1 ) {
        println("classifier predicted: " + classifier.predict(full(e(?,i).t)))
        println("actually was: " + l(i,0))
      }
    }
  }
