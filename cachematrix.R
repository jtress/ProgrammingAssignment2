## makeCacheMatrix and cacheSolve are a pair of functions to calculate, cache
## and return the inverse of a matrix

## makeCacheMatrix creates a list of the given matrix x which sets the matrix,
## returns the matrix, sets the inverse of the matrix and returns the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve is intended to take the list created by the makeCacheMatrix
## function and return the inverse of the matrix by calculating the inverse
## if not present or returning the cached inverse if it already exists in the
## list x. If the inverse was not found in the cache, it will be saved in the
## cache after it is calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
