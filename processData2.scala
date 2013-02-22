import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

package ProcessData2 {
  object DefaultProcess extends App {
    val dataDir = "/scratch/HW2/tokenized.mat"
    val tokens: IMat = load(dataDir, "tokens")
    val words: CSMat = load(dataDir, "smap")
    
    //one part of our final output
    var X: FMat = null
    //we build X row by row
    var row: FMat = null

    //iterate through columns
    for ( i <- 0 to tokens.ncols-1 ) {
      //if the token in the column is <review> make a new row
      if ( words(tokens(2,i)) == "<review>" ) {
        row = zeros(0, words.ncols)
      }
      //set the tokens position in the row to 1.0
      row(0,tokens(2,i)) = 1.0
      //if the token was </review> stack it on
      if ( words(tokens(2,i)) == "</review>" ) {
        if ( X == null) { X = row }
        else { X = X on row }
      }
    }
  }
}
