import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

import scala.util.Marshal
import scala.io.Source
import java.io._

object FirstPass {
  
  var dictionary: Set[String] = Set[String]()
  var wordBag: Map[Integer, Set[String]] = Map[Integer, Set[String]]()
  var labelBag: Map[Integer, Int] = Map[Integer, Int]()
  var tokenIndex: Map[Integer, String] = Map[Integer, String]()
  var revTokenIndex: Map[String, Integer] = Map[String, Integer]()

  def main(args: Array[String]) {
    firstPass()
    secondPass()
    thirdPass()
  }

  def firstPass() = {
    println("loading data...")
    var tokens: IMat = load("/scratch/HW2/tokenized.mat", "tokens")
    tokens = tokens.t(?, 2)
    val words: CSMat = load("/scratch/HW2/tokenized.mat", "smap")
    println("data loaded")
    
    var ratingFlag = false
    var reviewFlag = false 

    var review: Integer = 0
    for ( i <- 0 to tokens.size-1 ) {
      val index = tokens(i)-1
      if ( words(index) == "</review_text>" ) { 
        reviewFlag = false
        if (review%1000 == 0) println(review)
        review += 1
      }
      if ( ratingFlag ) {
        var l = 0
        var ll = words(index)
	if ( ll == "1" ) { l = 1 }
        else if ( ll == "2" ) { l = 2 }
        else if ( ll == "3" ) { l = 3 }
        else if ( ll == "4" ) { l = 4 }
        else if ( ll == "5" ) { l = 5 }
        labelBag += (review -> l)
        ratingFlag = false
      }
      if ( reviewFlag && index < 100000 ) {
        val ns: Set[String] = wordBag(review) + words(index)
        wordBag += ( review -> ns )
      }
      if ( words(index) == "<rating>" ) { ratingFlag = true }
      if ( words(index) == "<review_text>" ) { 
        reviewFlag = true 
        wordBag += ( review -> Set[String]() )
      }
    }
    for ( (k,v) <- wordBag ) dictionary = dictionary ++ v
  }

  def secondPass() = {
    println("book keeping...")
    var i: Integer = 0
    for ( x <- dictionary ) {
      //tokenIndex += ( i -> x )
      revTokenIndex += ( x -> i)
      i += 1
    }
    println("built dictionary of " + dictionary.size + " tokens.")
    println("collected " + wordBag.keys.size + " bags of words")
    println("gathered " + labelBag.keys.size + " ratings!")
  }
  
  def thirdPass() = {
    println("building Y...")
    var Y:IMat = icol(labelBag.values.toList)
    println("built Y vector")
    
    println("building X matrix...")
    var X: SMat = null
    for ( i <- 0 to 10000 ) { //wordBag.keys.size-1 ) {
      var ii: IMat = null
      for ( t <- wordBag(i) ) {
        if ( ii == null ) { ii = icol(revTokenIndex(t)) }
        else { ii = ii on revTokenIndex(t) }
      }
      val jj: IMat = IMat(wordBag(i).size, 1)
      val vv: FMat = ones(wordBag(i).size, 1)
      var c: SMat = null
      if ( ii == null ) { c = sparse(zeros(dictionary.size,1)) }
      else { c = sparse(ii, jj, vv, dictionary.size, 1) }
      if ( X == null ) { X = c }
      else { X = X \ c }
      if ( i%1000 == 0 ) { println("X is " + i + " cols") }
    }
    println("built X matrix")
    println("saving files")
    saveAs("TrimmedSparse.mat", X, "X", Y, "Y")
  }
}
