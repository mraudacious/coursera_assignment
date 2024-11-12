## The following functions work together to cache the inverse of a matrix.
## This caching mechanism avoids redundant calculations, saving time when the inverse
## of a matrix is needed multiple times.

## The makeCacheMatrix function creates a special "matrix" object that can store 
## the matrix itself and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL (no cached value)
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y  # Assigns the new matrix value in the parent environment
    inv <<- NULL  # Resets the inverse cache as the matrix has changed
  }
  
  # Function to get the current matrix
  get <- function() x
  
  # Function to set (cache) the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse, if it exists
  getInverse <- function() inv
  
  # Return a list of all defined functions for setting and getting
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The cacheSolve function computes the inverse of the special "matrix" created 
## by makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  # If cached inverse exists, retrieve it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If no cached inverse, calculate the inverse
  data <- x$get()  # Retrieve the matrix
  inv <- solve(data, ...)  # Calculate the inverse
  x$setInverse(inv)  # Cache the inverse
  
  inv  # Return the calculated inverse
}


