## Caching the Inverse of a Matrix
## Below are a pair of functions that are used to create a special object that
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invrs <<- inverse
    getInverse <- function() invrs
    list(set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
    
}


## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invrs <- x$getInverse()
    if (!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    mat <- x$get()
    invrs <- solve(mat, ...)
    x$setInverse(invrs)
    invrs
}

