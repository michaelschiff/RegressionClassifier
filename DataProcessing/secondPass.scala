import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

import scala.util.Marshal
import scala.io.Source
import java.io._

object FirstPass {
  def main(args: Array[String]) {
    var in = new FileInputStream("dictionaryBasic")
    var bytes = Stream.continually(in.read).takeWhile(-1 != ).map(_.toByte).toArray
    val dictionary: Set[String] = Marshal.load[Set[String]](bytes)
    println("loaded dictionary of " + dictionary.size + " terms.")

    in = new FileInputStream("wordBagBasic")
    bytes = Stream.continually(in.read).takeWhile(-1 != ).map(_.toByte).toArray
    val wordBag: Map[Integer, Set[String]] = Marshal.load[Map[Integer, Set[String]]](bytes)
    println("loaded  " + wordBag.keys.size + " bags of words.")

    in = new FileInputStream("labelBagBasic")
    bytes = Stream.continually(in.read).takeWhile(-1 != ).map(_.toByte).toArray
    val labelBag: Map[Integer, String] = Marshal.load[Map[Integer, String]](bytes)
    println("loaded  " + labelBag.keys.size + " ratings.")
  }
}
