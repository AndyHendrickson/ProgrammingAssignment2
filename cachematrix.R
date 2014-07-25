## Put comments here that give an overall description of what your
## functions do

## Returns a list of functions that give access to the specified matrix and
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # The matrix inverse default value (not calculated at initialization).
  inv_x <- NULL
  
  # Functions to access and modify x and its inverse.
  # Set a new matrix.
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  # Retrieve the current matrix.
  get <- function() x
  
  # Cache the inverse matrix.
  setinverse <- function(inverse) inv_x <<- inverse
  
  # Retrieve the cached inverse matrix.
  getinverse <- function() inv_x
  
  # Return a named list of these functions.
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the matrix described by the list of functions 'x'.
## Caches the inverse of x's matrix the first time it's called, and returns
## the cached copy thereafter.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.
  
  # Get the cached copy by calling the list's function.
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # There was no cached copy, so calculate the inverse and cache it.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  # Return the newly-caclulated inverse of x.
  m
}
