import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

import scala.util.Marshal
import scala.io.Source
import java.io._
import scala.collection.mutable.Map

FirstPass.main(Array())

object FirstPass {
  
  var dictionary: Set[String] = Set[String]()
  var wordBag: Map[Integer, Map[String, Integer]] = new Map[Integer, Map[String, Integer]]() // ( review -> ( word -> wordCount) )
  var labelBag: Map[Integer, Int] = Map[Integer, Int]()  // review num -> label
  var tokenIndex: Map[Integer, String] = Map[Integer, String]()
  var revTokenIndex: Map[String, Integer] = Map[String, Integer]()
  var stem = false
  val stemmer = new Stemmer()

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
        var wd = words(index)
        if (stem) { wd = stemmer.stem(wd) }
        if ( wordBag(review).values contains wd ) { wordBag(review)(wd) += 1 }
        else { wordBag(review)(wd) = 1 }
        dictionary = dictionary + wd
      }
      if ( words(index) == "<rating>" ) { ratingFlag = true }
      if ( words(index) == "<review_text>" ) { 
        reviewFlag = true 
        wordBag += ( review -> Map[String, Integer]() )
      }
    }
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
    for ( i <- 0 to wordBag.keys.size-1 ) {
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
      if ( (i+1)%1000 == 0 ) { 
        println("Saving " + (i+1)/1000.0 + "th partial X") 
        if (stem) { saveAs("StemmedOut/StemmedTrimmedSparse"+((i+1)/1000.0).toInt+".mat", X, ((i+1)/1000.0).toInt+"StemmedX") }
        else { saveAs("out/TrimmedSparse"+((i+1)/1000.0).toInt+".mat", X, ((i+1)/1000.0).toInt+"X") }
        X = null
      }
    }
    println("saving Y and last partial X")
    if (stem) {
      saveAs("StemmedOut/StemmedTrimmedSparseY.mat", Y, "Y")
      saveAs("StemmedOut/StemmedTrimmedSparseLastX.mat", X, "LastStemmedX")
    } else {
      saveAs("out/TrimmedSparseY.mat", Y, "Y")
      saveAs("out/TrimmedSparseLastX.mat", X, "LastX")
    }
  }
}

///Porter Stemmer
class Stemmer
{

  // word to be stemmed.
  var b = ""

  // Just recode the existing stuff, then go through and refactor with some intelligence.
  def cons( i: Int ): Boolean =
  {
    var ch = b( i )

    // magic!
    var vowels = "aeiou"

    // multi return. yuck
    if ( vowels.contains( ch ) )
      return false

    if ( ch == 'y'  )
    {
      if ( i == 0 )
      {
        return true
      }
      else
      {
        // loop it!
        return !cons( i - 1 )
      }
    }

    return true

  }

  // Add via letter or entire word
  def add( ch: Char ) =
  {
    b += ch
  }

  def add( word: String ) =
  {
    b = word
  }


  /* m() measures the number of consonant sequences between 0 and j. if c is
      a consonant sequence and v a vowel sequence, and <..> indicates arbitrary
      presence,

         <c><v>       gives 0
         <c>vc<v>     gives 1
         <c>vcvc<v>   gives 2
         <c>vcvcvc<v> gives 3
         ....
   *
   * I think this can be recoded far more neatly.
   */

   def calcM(s:String): Int =
   {
     var l = s.length
     var count = 0
     var currentConst = false

     for ( c <- 0 to l-1 )
     {
       if ( cons( c ) )
       {
         if (!currentConst && c != 0 )
         {
           count += 1
         }
         currentConst = true
       }
       else
       {
         currentConst = false
       }
     }

     return count
   }


   // removing the suffix 's', does a vowel exist?'
   def vowelInStem( s: String): Boolean =
   {

     for (i <- 0 to b.length -1 - s.length )
     {
       if ( !cons( i ) )
       {
         return true
       }
     }

     return false;

   }

   /* doublec(j) is true <=> j,(j-1) contain a double consonant. */
   def doublec( ): Boolean =
   {
     var l = b.length - 1

     if ( l < 1 )
       return false

     if ( b(l) != b(l-1) )
       return false

     return cons( l )

   }

   /* cvc(i) is true <=> i-2,i-1,i has the form consonant - vowel - consonant
      and also if the second c is not w,x or y. this is used when trying to
      restore an e at the end of a short word. e.g.

         cav(e), lov(e), hop(e), crim(e), but
         snow, box, tray.

   */

   def cvc( s:String): Boolean =
   {
     var i = b.length - 1 - s.length
     if (i < 2 || !cons(i) || cons(i-1) || !cons(i-2))
       return false;

     var ch = b(i)

     var vals = "wxy"

     if ( vals.contains( ch ) )
       return false

     return true;
   }


  // returns true if it did the change.
  def replacer( orig: String, replace:String, checker: Int => Boolean ): Boolean =
  {


    var l = b.length
    var origLength = orig.length
    var res = false

    if ( b.endsWith( orig ) )
    {
      var n = b.substring( 0, l - origLength  )

      var m = calcM( n )
      if ( checker( m ) )
      {
        b = n + replace
      }

      res = true

    }

    return res
  }

  // process the list of tuples to find which prefix matches the case.
  // checker is the conditional checker for m.
  def processSubList( l:List[(String, String)], checker: Int=>Boolean ): Boolean =
  {
    var iter = l.elements
    var done = false

    while (!done && iter.hasNext )
    {
      var v = iter.next
      done = replacer( v._1, v._2, checker )

    }

    return done
  }

  def step1()
  {

    var l = b.length

    var m = calcM( b )

    // step 1a
    var vals = List( ("sses", "ss"), ("ies","i"), ("ss","ss"), ("s", "") )
    processSubList( vals, _ >= 0)

    // step 1b
    if ( !(replacer( "eed", "ee", _>0) ) )
    {

      if ( ( vowelInStem("ed") && replacer("ed", "", _>=0) ) || ( vowelInStem("ing") && replacer( "ing", "", _>=0)  ) )
      {

        vals = List( ("at", "ate"), ("bl","ble"), ("iz","ize"))

        if (! processSubList( vals, _>=0 ) )
        {
          // if this isn't done, then it gets more confusing.

          m = calcM( b )
          var last = b( b.length -1 )
          if ( doublec() && !"lsz".contains( last ) )
          {
            b = b.substring( 0, b.length - 1 )
          }
          else
          if ( m == 1 && cvc("") )
          {
            b = b + "e"
          }
        }
      }
    }


    // step 1c

    ( vowelInStem("y") && replacer("y", "i", _>=0))

   }


   def step2( ) =
   {

      var vals = List( ("ational", "ate"),("tional","tion"),("enci","ence"),("anci","ance"),("izer","ize"),("bli","ble"),("alli", "al"),
                       ("entli","ent"),("eli","e"),("ousli","ous"),("ization","ize"),("ation","ate"),("ator","ate"),("alism","al"),
                       ("iveness","ive"),("fulness","ful"),("ousness", "ous"),("aliti", "al"),("iviti","ive"),("biliti", "ble"),("logi", "log"))

      processSubList( vals, _>0 )

   }

  def step3( ) =
  {

      var vals = List( ("icate", "ic"),("ative",""),("alize","al"),("iciti","ic"),("ical","ic"),("ful",""),("ness",""))

      processSubList( vals, _>0 )

  }

  def step4( ) =
  {

      // first part.
      var vals = List( ("al",""),("ance",""),("ence",""),("er",""),("ic",""),("able",""),("ible",""),("ant",""),("ement",""),
                       ("ment",""),("ent",""))

      var res = processSubList( vals, _>1 )

      // special part.
      if ( ! res )
      {
        if ( b.length > 4 )
        {
          if (  b( b.length - 4  ) == 's' || b( b.length -4 ) == 't' )
          {
            res = replacer("ion", "", _>1)

          }
        }

      }


      // third part.
      if ( !res )
      {
        var vals = List( ("ou",""),("ism",""),("ate",""),("iti",""),("ous",""),("ive",""),("ize",""))
        res = processSubList( vals, _>1 )

      }

  }

  def step5a( ) =
  {

      var res = false

      res = replacer("e", "", _>1)

      if ( !cvc("e") )
      {
        res = replacer("e", "", _==1)
      }


  }

  def step5b( ) =
  {

    var res = false
    var m = calcM( b )
    if ( m > 1 && doublec() && b.endsWith("l") )
    {
      b = b.substring(0, b.length - 1)
    }

  }

  def stem (str: String ): String  =
  {
     add(str)
     if ( b.length > 2 )
      {
        step1()
        step2()
        step3()
        step4()
        step5a()
        step5b()
      }
      return b
    }

}
