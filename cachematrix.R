## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special list object that can cache the inverse of a matrix. The list
## provides get, set, getInverse, setInverse function for working the matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) s<<- inverseMatrix
  getInverse <- function() s
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of matrix X and stores the results in cache. If the
## matrix that we want to compute the inverse is already in cache we used the computed
## inverse matrix instead of recomputing.

cacheInverseMatrix <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getInverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setInverse(s)
  s
}
