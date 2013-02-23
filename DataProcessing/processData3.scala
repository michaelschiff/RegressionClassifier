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
  var labelBag: Map[Integer, Float] = Map[Integer, Float]()
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
        var l = 0.0f
        var ll = words(index)
        if ( ll == "1.0" ) { l = 1.0f }
        else if ( ll == "2.0" ) { l = 2.0f }
        else if ( ll == "3.0" ) { l = 3.0f }
        else if ( ll == "4.0" ) { l = 4.0f }
        else if ( ll == "5.0" ) { l = 5.0f }
        labelBag += (review -> l)
        ratingFlag = false
      }
      if ( reviewFlag ) {
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
    }
    println("built dictionary of " + dictionary.size + " tokens.")
    println("collected " + wordBag.keys.size + " bags of words")
    println("gathered " + labelBag.keys.size + " ratings!")
  }
  
  def thirdPass() = {
    println("building Y...")
    var Y:FMat = FMat(icol(labelBag.values.map( a => a.toInt ).toList))
    println("built Y vector")
    
    println("building X matrix...")
    var X: SMat = null
    for ( i <- 0 to wordBag.keys.size-1 ) {
      var ii: IMat = null
      for ( t <- wordBag(i) ) {
        if ( ii == null ) { ii = icol(revTokenIndex(t)) }
        else { ii = ii on icol(revTokenIndex(t)) }
      }
      val jj: IMat = IMat(zeros(wordBag(i).size, 1)) //always 0 column
      val vv: FMat = ones(wordBag(i).size, 1) //turn on bit given by row,col
      val col: SMat = sparse(ii, jj, vv, dictionary.size, 1)
      if ( X == null ) { X = col }
      else { X = X \ col }
      if ( i%1000 == 0 ) { println("X is " + i + " cols") }
    }
    println("built X matrix")
    println("saving files")
    saveAs("mats.out", X, "X", Y, "Y")
  }
}
