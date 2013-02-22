import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

object DefaultProcess {
  def main(args: Array[String]) {
    println("loading data...")
    var tokens: IMat = load("/scratch/HW2/tokenized.mat", "tokens")
    val words: CSMat = load("/scratch/HW2/tokenized.mat", "smap")
    println("building matrices...")
    var Y: FMat = null
    var X: SMat = null
    var col: FMat = zeros(words.nrows,1)
    
    var flag = false

    for ( i <- 2 to tokens.size-1 by 3 ) {
      val index = tokens(i)-1
      col(index) = 1.0
      if ( flag ) {
        var label = 0.0
        val w = words(index)
        if (w == "1.0") { label = 1.0 }
        else if (w == "2.0") { label = 2.0 }
        else if (w == "3.0") { label = 3.0 }
        else if (w == "4.0") { label = 4.0 }
        else if (w == "5.0") { label = 5.0 }
        if ( Y == null ) { Y = label }
        else { Y = Y on label }
        flag = false
      }
      if ( words(index) == "<rating>" ) { flag = true }
      if ( words(index) == "</review>" ) {
        if ( X == null ) { X = sparse(col) }
        else { X = X \ sparse(col) }
        col = zeros(words.nrows,1)
        println(X.ncols)
      }
    }
    saveAs("out.mat", X, "X", Y, "Y")
  }
}
