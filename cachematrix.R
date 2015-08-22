## The purpose of these functions is to create a matrix
## wrapper which can cache its inverse and the cached
## value when available

## Creates a matrix wrapper which has the ability to
## cache the inverse of itself
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


## Return the inverse of special matrix parameter 'x'
## created with 'makeCacheMatrix'. Checks if the
## inverse is cached already before calculating and
## caching it.
cacheSolve <- function(x, ...) {
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