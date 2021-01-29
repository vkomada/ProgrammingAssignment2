## This function serves as a means to cache the inverse of a matrix.

## the makeCacheMatrix() is a function that creates a list of functions that
## allow the user to create a matrix object and then later cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## the cacheSolve() function takes the list of functions from makeCacheMatrix ()
## and solves it, computing the inverse of the matrix using solve().
## This relies on the concept of S3 objects (objects that produce a list of
## functions within a function).
## If the inverse has already been calculated, cacheSolve() will
## retrieve the previously cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  return(inv)
}
