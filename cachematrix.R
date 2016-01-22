## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) s<<- solve
  getmatrix <- function() s
  list(set = set, get = get,setmatrix = setmatrix,getmatrix = getmatrix)
}


## Write a short comment describing this function
## This function caches the inverse of matrix X and stores the results in cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getmatrix()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setmatrix(s)
  s
}
