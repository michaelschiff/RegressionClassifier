import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._
import java.io._

package MatrixBuilder {
object Builder extends App{
  
	println("started")
			
	var a = new DataInputStream(new FileInputStream("tokens.bin"));
	var end = false
	//Number of features
	var threshold = 40000
	//Number of documents
	var documents = 1000
	var totalDocuments = 975194;
	var matrix = FMat(threshold, documents);
	var score = FMat(totalDocuments, 1);
	var currentDocument = -1
	var nextIsIndex = false;
	var iteration = 0;
	var count = 0;
	//Iterates through the data set
	while(!end) {
		  try{
		    //Reads the next three bites of data, byte 1 is row, 2 is position, 3 is the actual word.
		    var b = java.lang.Integer.reverseBytes(a.readInt())
		    var c = java.lang.Integer.reverseBytes(a.readInt())
		    var d = java.lang.Integer.reverseBytes(a.readInt())
		    //If the word is <Review> start a new document
		    if(d == 24) {
		      currentDocument +=1;
		      //If there are two many documents, save the documents and restart the count at 0
		      if(currentDocument == documents){
		    	saveAs("out" + iteration + ".mat", matrix, "tokens")
		    	matrix = FMat(threshold, documents);
		        iteration += 1
		    	println(iteration)
		        currentDocument = 0
		      }
		    }
		    //Score the documents
		    if(nextIsIndex){
		      nextIsIndex = false;
		      //There's a better way to do this.
		      var tempScore = 0
		      if(d == 72) tempScore = 1
		      if(d == 78) tempScore = 2
		      if(d == 104) tempScore = 3
		      if(d == 71) tempScore = 4
		      if(d == 48) tempScore = 5
		      if(d == 21) tempScore = 0
		    
		      score(currentDocument + documents*iteration, 0) = tempScore
		    }
		    //This signals that the next word is a score.
		    if(d == 31) nextIsIndex = true;
		    //If the word in the document is less then the threshold (e.g. frequent enough), 
		    //then add to the matrix.
		    if(d < threshold) matrix(d-1, currentDocument) = 1;
		    
		  }
		  //If there is an exception, end. (Will throw an excpetion at the end of the document)
		  catch{
		    case e:EOFException => end = true
		    case e:Exception => end = true
		  }
	}
	//Save the final array (may be some zeroes at the end...)
	saveAs("out" + iteration + ".mat", matrix, "tokens")
	//save the scores.
	saveAs("scores.mat", score, "tokens")
	print("end")
}
}
