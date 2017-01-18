## Matrix inversion is usually a costly computation and there may be benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.
## This pair of functions caches the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## Create a special "matrix" object that can cache its inverse.
    
    x_inv <- NULL
    set <- function(y) {         # cacheSolve works without the "set" function
        x <<- y                  # but "set" adds extra functionality
        x_inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) x_inv <<- solve
    getinv <- function() x_inv
    list(set = set, get = get, 
        setinv = setinv,
        getinv = getinv)
}

## The cacheSolve function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinv()
    if(!is.null(x_inv)) {
        message("getting cached inverse")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data, ...)
    x$setinv(x_inv)
    x_inv
}
