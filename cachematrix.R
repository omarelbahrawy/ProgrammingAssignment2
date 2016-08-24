## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setvalue <- function(y) {
        x <<- y
        i <<- NULL
    }
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    getvalue <- function() x
    list(setvalue = setvalue, getvalue = getvalue, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated , then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$getvalue()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
