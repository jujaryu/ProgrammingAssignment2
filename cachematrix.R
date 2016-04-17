## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        re <- NULL
        set <- function(y) {
                x <<- y
                re <<- NULL
        }
        get <- function() x
        setReverse <- function(reverse) re <<- reverse
        getReverse <- function() re
        list(set = set,
             get = get,
             setReverse = setReverse,
             getReverse = getReverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        re <- x$getReverse()
        if (!is.null(re)) {
                return(re)
        }
        mt <- x$get()
        re <- solve(mt)
        x$setReversee(re)
        re
}
