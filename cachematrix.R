## This R file contains two functions:
## - makeCacheMatrix(x) a wrapper for matrix that returns a list of functions
## - cacheSolve(x) a function that computes the inverse matrix taken in input
##    the list returned by the function makeCacheMatrix. The inverse matrix
##    calculation is performed only the first time, all the subsequent call are
##    based on the value stored.

## Function makeCacheMatrix takes a square matrix x. An error will be returned
## if x is not a square matrix. The function return a list of function on x:
##  - set:  set (or reset) the matrix
##  - get: get the matrix
##  - setInv: set the inverse matrix
##  - getInv: get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  if(nrow(x) != ncol(x))
    stop("Matrix x must be square")
  
  inv <- NULL
  set <- function(y){
    x <<-- y
    inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inv) inv <<- inv
  getInv <- function() inv
  list(set= set, get = get, setInv = setInv, getInv = getInv)
}


## This function takes a makeCacheMatrix object and performs the inverse matrix
## calculation. No calculation is performed if the inverse matrix is already
## available
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  
  ## If inverse matrix is not cached, then perform calculation
  if(is.null(inv)){
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInv(inv)
  }
  inv
}
