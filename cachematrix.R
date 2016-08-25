## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## which is really a list containing a function to
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the matrixInverse
## 4- get the value of the matrixInverse


makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    setvalue <- function(value) {
        x <<- value
        matrixInverse <<- NULL
    }
    setinverse <- function(solve) matrixInverse <<- solve
    getinverse <- function() matrixInverse
    getvalue <- function() x
    list(setvalue = setvalue, getvalue = getvalue, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated , then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$getvalue()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
