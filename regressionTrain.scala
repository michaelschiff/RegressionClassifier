import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

package RegressionTrainer {
  class trainer(X: FMat, Y: FMat, a: Float, t: Float) {
    Mat.noMKL=true
    val THRESHOLD: Float = .0000000001f
    val ALPHA: Float = .001f
    var w: FMat = zeros(X.nrows ,1)
    def gradients(): FMat = {
      val combo = (w.t * X).t
      val diff = combo - Y
      val twice_diff = diff * 2.0f
      //var gs: FMat = null
      //for ( i <- 0 to X.nrows-1 ) { 
      //  val g = X(i, ?) * twice_diff
      //  if (gs == null) { gs = g }
      //  else { gs = gs on g}
      //}
      var gs = X * twice_diff
      return gs
    }
    def error(): Float = {
      val k: FMat = gradients()
      val e: Float = abs(sum(k,1))(0,0)
      return e
    }
    while ( error() > THRESHOLD ) {
      w -= gradients() * ALPHA
    }
  }
}
