## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Setter function to set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Getter function to get the matrix
  get <- function() x
  
  # Getter function to get the cached inverse
  getInverse <- function() inverse
  
  # Setter function to cache the inverse
  setInverse <- function(inverseMatrix) {
    inverse <<- inverseMatrix
  }
  
  # Return a list of functions
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse if available
  inverse <- x$getInverse()
  
  # If the inverse is not cached, compute it and cache it
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  
  # If the inverse is not cached, compute it using solve() and cache it
  matrixData <- x$get()
  inverse <- solve(matrixData, ...)
  x$setInverse(inverse)
  inverse
}

