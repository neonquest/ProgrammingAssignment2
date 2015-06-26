## The following functions allow to cache inverse of a matrix.
## Usage:
## 1. Use 'makeCacheMatrix' to create a special "matrix" object from an 
##    existing matrix object.
##      x <- makeCacheMatrix(m)
##
## 2. Use 'cacheSolve' to get the inverse of matrix 'm' by passing the special 
##    "matrix" object 'x' from 1) above. This will ensure the inverse of 'm' is
##    computed only once and returned from a cache for all subsequent calls.
##      inv <- cacheSolve(x)


## Function creates a special "matrix" object, which is a list containing 
## functions to get/set value of the matrix, get/set inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Function to set value of the matrix
    # Resets value of inv as it needs to be computed for the new matrix value
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Function to return value of the matrix
    get <- function() x
    
    # Function to set the inverse value
    setinv <- function(inverse) {
        inv <<- inverse
    }
    
    # Function to return inverse value
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function that returns a inverse of matrix either by computing if it wasn't 
## done earlier or by getting the cached value in the special "matrix" object

cacheSolve <- function(x, ...) {
    # Get cached inverse from special "matrix" object
    inv <- x$getinv()
    
    # If cached inverse value exists, return it
    if (!is.null(inv)) {
        message("Getting cached inverse value")
        return(inv)
    }
    
    # Get the matrix value for which inverse needs to be computed
    m <- x$get()
    
    # Compute the inverse for the first time
    inv <- solve(m, ...)
    
    # Store the inverse in the special "matrix" object
    x$setinv(inv)
    
    # Return inverse value
    inv
}
