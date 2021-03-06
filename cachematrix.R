## Caching the inverse of a matrix

## Below are two functions that are used to create a special
## object that stores a matrix and that caches its inverse. 
## These functions make use of the <<- operator to assign a
## value to an object in an environment that is different from
## the current environment.

## The first function 'makeCacheMatrix' creates a special
## "matrix" object that can cache its inverse.

## The special "matrix" object created by the `makeCacheMatrix` 
## function is a list containing a function to
## 1.  set the values of the matrix
## 2.  get the values of the matrix
## 3.  set the values of the inverse matrix
## 4.  get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix(nrow = 0, ncol = 0)) {
    m <- NULL
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMatrix  <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(setMatrix  = setMatrix, 
         getMatrix  = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)  
}

## The second function 'cacheSolve' computes the inverse of
## the special "matrix" created with the above function
## 'makeCacheMatrix'. The inverse is computed with the 'solve' 
## function (i.e. `solve(x)` returns the inverse of a square 
## invertible matrix 'x'). However, 'cacheSolve' first checks
## to see if the inverse has already been calculated. If so,
## it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets
## the values of the inverse in the cache via the 'setInverse'
## function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getMatrix()
    m    <- solve(data, ...)
    x$setInverse(m)
    m
}