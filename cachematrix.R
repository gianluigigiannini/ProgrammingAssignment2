## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##the "makeCacheMatric" function corresponds to the "makeVector" function 
##in the assignment description: it creates basically a list, which contains
##a function to set/get the value of the matrix, and set/get the inverse of the matrix.

makeCacheMatrix <- function(x=matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) inv <<- solve
    getSolve <- function() inv
    list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}

## Write a short comment describing this function

## the "cacheSolve" function computes the inverse of the special "matrix" created
## by the function "makeCacheMatrix". If the inverse had already been calculated
## (and it has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getSolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setSolve(inv)
    inv
}

## Return a matrix that is the inverse of 'x'