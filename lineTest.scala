import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

import RegressionTrainer._

package LineTest { 
  object LineTest extends App {

    var x = (1 on 1 on 1) \ (1 on 2 on 1) \ (1 on 3 on 1)
    var y = 2 on 4 on 6

    var t = new RegressionTrainer.trainer(x, y, 0.001f, 0.00000000001f)

    println(t.w)
  }
}
