## This script contains functions for computing the inverse 
## of a square matrix and cache the result. 
## If the inverse of the matrix has already been computed 
## (and cached), it will return the pre-computed inverse.

## makeCacheMatrix creates a special "matrix object" (a list), 
## that contains functions to set and get the matrix itself, 
## as well as to set and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Create the function for setting the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Create the function returning the matrix
  get <- function() x
  
  ## Create the function for setting the value of the inverse
  setinv <- function(inverse) inv <<- inverse
  
  ## Create the function returning the inverse
  getinv <- function() inv
  
  ## Return a list containing the constructed functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse of x, which should 
## be an object of the type returned from makeCacheMatrix.
## If the inverse is already computed (and cached),
## the pre-computed value is returned.

cacheSolve <- function(x, ...) {
  ## Get the inverse of x from the cache
  inv <- x$getinv()
  
  ## If the inverse existed, return the cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If the inverse did not exist, get the matrix from x and 
  ## compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  ## Cache the value of the inverse
  x$setinv(inv)
  
  ## Return the inverse
  inv
}
