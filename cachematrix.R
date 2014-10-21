## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. This project containts a pair of functions that
## cache the inverse of a matrix.

## The makeCacheMatrix function creates a list object containing four function
##  One to set the matrix, one to get the matrix
##  One to set the inverse and one to get the inverse

makeCacheMatrix <- function(x = matrix()) {

    myInv <- NULL
    set <- function(y) {
        x <<- y
        myInv <<- NULL
    }
    get <- function() x
    setInv <- function(InvCalc) myInv <<- InvCalc
    getInv <- function() myInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## The cacheSolve function calculates the mean of the special "vector" created
## with the makeCacheMatrix function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of 
## the data and sets the value of the mean in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    
    myInv <- x$getInv()
    if(!is.null(myInv)) {
        message("getting cached data")
        return(myInv)
    }
    data <- x$get()
    myInv <- solve(data, ...)
    x$setInv(myInv)
    myInv
}
