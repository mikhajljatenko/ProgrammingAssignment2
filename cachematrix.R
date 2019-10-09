## The makeCacheMatrix function creates a special matrix, which has a set of functions
## to set and get the value of the matrix, and to get and set the inverse of that
## matrix. cacheSolve calculates the inverse for this special matrix object, and
## caches the result. Once the result is cached, it returns the cached result till
## the underlying matrix object remains the same. It recomputes and recaches the
## result if the underlying matrix object changes.

## Create a special matrix object that is capable of caching it's inverse

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrixInverse <<- inverse
    getinverse <- function() matrixInverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return the cached inverse for the special matrix object, if the matrix
## has not changed, and the inverse for that already exists, compute otherwise and
## update the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## For this assignment we can assume that matrix 'x' is always
    ## invertible, so no need to put any checks for that
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
