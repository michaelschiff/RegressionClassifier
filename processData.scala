import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

package ProcessData {
  object ProcessData extends App {
    val a:IMat = load("/Users/michaelschiff/Documents/Berkeley/Year 5/Data_Mining/assignment2/tokenized.mat","tokens")
  }
}
