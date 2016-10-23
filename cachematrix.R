## R Programming Assignment 2: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The functions below compute and cache the inverse of a matrix once,
## and return the cached inverse if it has already been computed.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    stopifnot(class(x) == "matrix", nrow(x)==ncol(x))
    inv_x <- NULL
    set <- function(y) {
        stopifnot(class(y) == "matrix", nrow(x)==ncol(x))
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv_x <<- inv
    getinv <- function() inv_x
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## The function cacheSolve() returns a matrix that is the inverse of the special "matrix" object 'x'
## which has been created by makeCacheMatrix().
cacheSolve <- function(x, ...) 
{
    inv_x <- x$getinv()
    if(!is.null(inv_x)) 
    {
        message("getting cached data")
    }
    else
    {
        message("calculating inverse")
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinv(inv_x)
    }
    inv_x
    
    
}
