## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). Your assignment is to write a pair of functions that 
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inverted = NULL
    
    set <- function(mtrx) {
        inverted <<- mtrx
    }
    
    get <- function() {
        x
    }
    
    setInv <- function(inv) {
        inverted <<- inv
    }
    
    getInv <- function() {
        inverted
    }
    
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cached <- x$getInv()
    
    if (!is.null(cached)) {
        message("Getting cached data.")
        return(cached)
    }
    
    original <- x$get()
    inverted <- solve(original, ...)
    x$setInv(inverted)
    
    inverted
}
