import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

  // X : a matrix of examples.  Each column is an example, each row is a feature
  // Y : a column vector of labels.  ith row is the label for the ith col of X
  // a : the step size
  // t : how much error we are willing to accept
  class trainer(X: FMat, Y: FMat, a: Float, t: Float) {
    Mat.noMKL=true
    val THRESHOLD: Float = t
    val ALPHA: Float = a
    var w: FMat = zeros(X.nrows ,1)
    def gradients(): FMat = {
      val combo = (w.t * X).t
      val diff = combo - Y
      val twice_diff = diff * 2.0f
      var gs = X * twice_diff
      return gs
    }
    def error(): Float = {
      val k: FMat = gradients()
      val e: Float = abs(sum(k,1))(0,0)
      return e
    }
    def predict(x: FMat): Float = x*w(0,0)
    while ( error() > THRESHOLD ) {
      w -= gradients() * ALPHA
    }
  }

  object trainAndTest {
    def main(args: Array[String]) {
      val e: SMat = load("out.mat", "X")
      val l: FMat = load("out.mat", "Y")
      val trainingExamples: FMat = full(e(?, 0 to 800000))
      val trainingLabels: FMat = (l.t)(?, 0 to 800000).t
      val testExamples: FMat = full(e(?, 800001 to ))
      val testLabels: FMat = (l.t)(?, 800001 to ).t
      
      val classifier = new trainer(trainingExamples, trainingLabels, 0.001f, 0.0000001f)
      for (i <- 0 to testExamples.ncols-1) {
        print("classifier predicted: ")
        print(classifier.predict(testExamples(?, i).t))
        print(" actually label was: ")
        println(testLabels(i, 0))
      }
    }
  }
