## These 2 functions, makeCacheMatrix and cacheSolve, are handling the caching of possibly-large matrices and their inversions. 
## Functionalities include:
## - storing a matrix in cache and retreiving it
## - retrieving its invertion from cache (first creating it if does not exist)

## The makeCacheMatrix function accepts as an argument matrix x; 
## usage examples:
##    m <- makeCacheMatrix(largeMatrix)   will store it in cache
##    m$getinverse()  will return the inverse matrix (or NULL if not yet established)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve creates the inverse of matrix argument x, whie first creating it if needed
## usage example:
##    cacheSolve(m)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
	    if(!is.null(m)) {
	        message("getting cached data")
	        return(m)
	    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
