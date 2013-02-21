RegressionClassifier
====================

An OLS regression classifier used to predict the number of stars given to an Amazon product review.  Written in Scala using the BIDMat library.

Input is specified by two matrixes.  one, X, is M x N (where M is the dimensionality of the feature vectors) and N is the number of examples.  Each column is an example, and the ith row of each example is the value of the ith feature.  
The second, Y, is M x 1 and specifies the labels for each example.  The ith row of this column vector corresponds to the label for the ith example (the ith column of X).

we learn the set of weights that best combines the examples to give the labels via stochastic gradient descent.

we multiply w * X to get our label for each doc
we subtract Y from this to get the difference.
we multiply this by two
we then multiply X * prevColumn -> yeilds a column vector (M x 1) where the ith row is the sum of the partial derivative of loss (wrt our current ith weight) at every example.

we subtract this column from w, moving it closer to the minimum of loss function.

when we are within some error threshold (the sum of the partial derivatives is close enough to zero, aka w is almost at its minimum) we decide that we have converged.

Our w vector can now be used to make a new prediction.  Given a new feature vector (M x 1), we can multiple w.t * fv to get our predicted label.


Use as follows:
  * use ProcessData to generate and serialize the input matrixes (X and Y)
  * use TrainingHandler (not written yet) to load the input matrixes, pass the input to regressionTrainer to learn the weights, and serialize the weight vector
  * use TestHandler (not written yet) to load in the weight vector and make predictions about the held out data
