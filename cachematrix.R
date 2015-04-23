## These functions  can calculate the inverse of a square matrix
## object and cache the result to avoid repeteadly calculations



## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(mInv) m <<- mInv
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mInv <- x$getInverse()
    if(!is.null(mInv)) {
        message("getting cached data")
        return(mInv)
    }
    data <- x$get()
    mInv <- solve(data, ...)
    x$setInverse(mInv)
    mInv
}


