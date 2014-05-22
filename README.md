## This is 'Programming Assignment 2: Lexical Scoping' for the Coursera R Programming course.

The assignment was write a pair of functions that cache the inverse of a matrix, using the makevector and the cachemean functions designed to cache the mean of a vector as examples (see 'ProgrammingAssignment2Instructions.md').

The task was to write the following functions:

1.  `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
2.  `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

Computing the inverse of a square matrix can be done with the `solve` function in R. For example, if `X` is a square invertible matrix, then `solve(X)` returns its inverse.

For this assignment, assume that the matrix supplied is always
invertible.

The functions should be saved in the "cachematrix.R" file.

In order to complete this assignment, the following must be done:

1.  Fork the GitHub repository containing the stub R files at [https://github.com/rdpeng/ProgrammingAssignment2](http://github.com/rdpeng/ProgrammingAssignment2) to create a copy under your own account.
2.  Clone your forked GitHub repository to your computer so that you can edit the files locally on your own machine.
3.  Edit the R file contained in the git repository and place your solution in that file (please do not rename the file).
4.  Commit your completed R file into YOUR git repository and push your git branch to the GitHub repository under your account.
5.  Submit to Coursera the URL to your GitHub repository that contains the completed R code for the assignment.

### Grading

This assignment will be graded via peer assessment.  See the rubric on Coursera.
