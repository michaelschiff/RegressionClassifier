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
  }

  def firstPass() = {
    println("loading data...")
    var tokens: IMat = load("/scratch/HW2/tokenized.mat", "tokens")
    val words: CSMat = load("/scratch/HW2/tokenized.mat", "smap")
    println("data loaded")
    
    var ratingFlag = false
    var reviewFlag = false 

    var review: Integer = 0
    for ( i <- 2 to tokens.size-1 by 3 ) {
      val index = tokens(i)-1
      if ( words(index) == "</review_text>" ) { 
        reviewFlag = false
        if (review%100 == 0) println(review)
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
    var i: Integer = 0
    for ( x <- dictionary ) {
      tokenIndex += ( i -> x )
      revTokenIndex += ( x -> i)
    }
    println("built dictionary of " + dictionary.size + " tokens.")
    println("collected " + wordBag.keys.size + " bags of words")
    println("gathered " + labelBag.keys.size + " ratings!")
  }

}
